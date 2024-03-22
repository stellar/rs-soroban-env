//! This module contains the [Storage] type and its supporting types, which
//! provide the [Host](crate::Host) with access to durable ledger entries.
//!
//! For more details, see the [Env](crate::Env) data access functions:
//!   - [Env::has_contract_data](crate::Env::has_contract_data)
//!   - [Env::get_contract_data](crate::Env::get_contract_data)
//!   - [Env::put_contract_data](crate::Env::put_contract_data)
//!   - [Env::del_contract_data](crate::Env::del_contract_data)

use std::rc::Rc;

use crate::{
    budget::Budget,
    host::metered_map::MeteredOrdMap,
    ledger_info::get_key_durability,
    xdr::{ContractDataDurability, LedgerEntry, LedgerKey, ScErrorCode, ScErrorType},
    Env, Error, Host, HostError, Val,
};

pub type FootprintMap = MeteredOrdMap<Rc<LedgerKey>, AccessType, Budget>;
pub type EntryWithLiveUntil = (Rc<LedgerEntry>, Option<u32>);
pub type StorageMap = MeteredOrdMap<Rc<LedgerKey>, Option<EntryWithLiveUntil>, Budget>;

/// The in-memory instance storage of the current running contract. Initially
/// contains entries from the `ScMap` of the corresponding `ScContractInstance`
/// contract data entry.
#[derive(Clone, Hash)]
pub(crate) struct InstanceStorageMap {
    pub(crate) map: MeteredOrdMap<Val, Val, Host>,
    pub(crate) is_modified: bool,
}

impl InstanceStorageMap {
    pub(crate) fn from_map(map: Vec<(Val, Val)>, host: &Host) -> Result<Self, HostError> {
        Ok(Self {
            map: MeteredOrdMap::from_map(map, host)?,
            is_modified: false,
        })
    }
}

/// A helper type used by [Footprint] to designate which ways
/// a given [LedgerKey] is accessed, or is allowed to be accessed,
/// in a given transaction.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum AccessType {
    /// When in [FootprintMode::Recording], indicates that the [LedgerKey] is only read.
    /// When in [FootprintMode::Enforcing], indicates that the [LedgerKey] is only _allowed_ to be read.
    ReadOnly,
    /// When in [FootprintMode::Recording], indicates that the [LedgerKey] is written (and also possibly read)
    /// When in [FootprintMode::Enforcing], indicates that the [LedgerKey] is _allowed_ to be written (and also allowed to be read).
    ReadWrite,
}

/// A helper type used by [FootprintMode::Recording] to provide access
/// to a stable read-snapshot of a ledger.
/// The snapshot is expected to only return live ledger entries.
#[cfg(any(test, feature = "unstable-next-api"))]
pub trait SnapshotSource {
    /// Returns the ledger entry for the key and its live_until ledger if entry
    /// exists, or `None` otherwise.
    fn get(&self, key: &Rc<LedgerKey>) -> Result<Option<EntryWithLiveUntil>, HostError>;
}

#[cfg(not(any(test, feature = "unstable-next-api")))]
pub trait SnapshotSource {
    // Returns the ledger entry for the key and its live_until ledger.
    fn get(&self, key: &Rc<LedgerKey>) -> Result<EntryWithLiveUntil, HostError>;
    fn has(&self, key: &Rc<LedgerKey>) -> Result<bool, HostError>;
}

/// Describes the total set of [LedgerKey]s that a given transaction
/// will access, as well as the [AccessType] governing each key.
///
/// A [Footprint] must be provided in order to run a transaction that
/// accesses any [LedgerKey]s in [FootprintMode::Enforcing]. If a
/// transaction has an unknown [Footprint] it can be calculated by
/// running a "preflight" execution in [FootprintMode::Recording],
/// against a suitably fresh [SnapshotSource].
// Notes on metering: covered by the underneath `MeteredOrdMap`.
#[derive(Clone, Default, Hash)]
pub struct Footprint(pub FootprintMap);

impl Footprint {
    pub fn record_access(
        &mut self,
        key: &Rc<LedgerKey>,
        ty: AccessType,
        budget: &Budget,
    ) -> Result<(), HostError> {
        if let Some(existing) = self.0.get::<Rc<LedgerKey>>(key, budget)? {
            match (existing, ty) {
                (AccessType::ReadOnly, AccessType::ReadOnly) => Ok(()),
                (AccessType::ReadOnly, AccessType::ReadWrite) => {
                    // The only interesting case is an upgrade
                    // from previously-read-only to read-write.
                    self.0 = self.0.insert(Rc::clone(key), ty, budget)?;
                    Ok(())
                }
                (AccessType::ReadWrite, AccessType::ReadOnly) => Ok(()),
                (AccessType::ReadWrite, AccessType::ReadWrite) => Ok(()),
            }
        } else {
            self.0 = self.0.insert(Rc::clone(key), ty, budget)?;
            Ok(())
        }
    }

    pub fn enforce_access(
        &mut self,
        key: &Rc<LedgerKey>,
        ty: AccessType,
        budget: &Budget,
    ) -> Result<(), HostError> {
        // `ExceededLimit` is not the most precise term here, but footprint has
        // to be externally supplied in a similar fashion to budget and it's
        // also representing an execution resource limit (number of ledger
        // entries to access), so it might be considered 'exceeded'.
        // This also helps distinguish access errors from the values simply
        // being  missing from storage (but with a valid footprint).
        if let Some(existing) = self.0.get::<Rc<LedgerKey>>(key, budget)? {
            match (existing, ty) {
                (AccessType::ReadOnly, AccessType::ReadOnly) => Ok(()),
                (AccessType::ReadOnly, AccessType::ReadWrite) => {
                    Err((ScErrorType::Storage, ScErrorCode::ExceededLimit).into())
                }
                (AccessType::ReadWrite, AccessType::ReadOnly) => Ok(()),
                (AccessType::ReadWrite, AccessType::ReadWrite) => Ok(()),
            }
        } else {
            Err((ScErrorType::Storage, ScErrorCode::ExceededLimit).into())
        }
    }
}

#[derive(Clone, Default)]
pub enum FootprintMode {
    Recording(Rc<dyn SnapshotSource>),
    #[default]
    Enforcing,
}

/// A special-purpose map from [LedgerKey]s to [LedgerEntry]s. Represents a
/// transactional batch of contract IO from and to durable storage, while
/// partitioning that IO between concurrently executing groups of contracts
/// through the use of IO [Footprint]s.
///
/// Specifically: access to each [LedgerKey] is mediated by the [Footprint],
/// which may be in either [FootprintMode::Recording] or
/// [FootprintMode::Enforcing] mode.
///
/// [FootprintMode::Recording] mode is used to calculate [Footprint]s during
/// "preflight" execution of a contract. Once calculated, a recorded [Footprint]
/// can be provided to "real" execution, which always runs in
/// [FootprintMode::Enforcing] mode and enforces partitioned access.
#[derive(Clone, Default)]
pub struct Storage {
    pub footprint: Footprint,
    pub mode: FootprintMode,
    pub map: StorageMap,
}

// Notes on metering: all storage operations: `put`, `get`, `del`, `has` are
// covered by the underlying [MeteredOrdMap] and the [Footprint]'s own map.
impl Storage {
    /// Only a subset of Stellar's XDR ledger key or entry types are supported
    /// by Soroban: accounts, trustlines, contract code and data. The rest are
    /// never used by stellar-core when interacting with the Soroban host, nor
    /// does the Soroban host ever generate any. Therefore the storage system
    /// will reject them with [ScErrorCode::InternalError] if they ever occur.
    pub fn check_supported_ledger_entry_type(le: &LedgerEntry) -> Result<(), HostError> {
        use crate::xdr::LedgerEntryData::*;
        match le.data {
            Account(_) | Trustline(_) | ContractData(_) | ContractCode(_) => Ok(()),
            Offer(_) | Data(_) | ClaimableBalance(_) | LiquidityPool(_) | ConfigSetting(_)
            | Ttl(_) => Err((ScErrorType::Storage, ScErrorCode::InternalError).into()),
        }
    }

    /// Only a subset of Stellar's XDR ledger key or entry types are supported
    /// by Soroban: accounts, trustlines, contract code and data. The rest are
    /// never used by stellar-core when interacting with the Soroban host, nor
    /// does the Soroban host ever generate any. Therefore the storage system
    /// will reject them with [ScErrorCode::InternalError] if they ever occur.
    pub fn check_supported_ledger_key_type(lk: &LedgerKey) -> Result<(), HostError> {
        use LedgerKey::*;
        match lk {
            Account(_) | Trustline(_) | ContractData(_) | ContractCode(_) => Ok(()),
            Offer(_) | Data(_) | ClaimableBalance(_) | LiquidityPool(_) | ConfigSetting(_)
            | Ttl(_) => Err((ScErrorType::Storage, ScErrorCode::InternalError).into()),
        }
    }

    /// Constructs a new [Storage] in [FootprintMode::Enforcing] using a
    /// given [Footprint] and a storage map populated with all the keys
    /// listed in the [Footprint].
    pub fn with_enforcing_footprint_and_map(footprint: Footprint, map: StorageMap) -> Self {
        Self {
            mode: FootprintMode::Enforcing,
            footprint,
            map,
        }
    }

    /// Constructs a new [Storage] in [FootprintMode::Recording] using a
    /// given [SnapshotSource].
    pub fn with_recording_footprint(src: Rc<dyn SnapshotSource>) -> Self {
        Self {
            mode: FootprintMode::Recording(src),
            footprint: Footprint::default(),
            map: Default::default(),
        }
    }

    // Helper function the next 3 `get`-variants funnel into.
    fn try_get_full(
        &mut self,
        key: &Rc<LedgerKey>,
        budget: &Budget,
    ) -> Result<Option<EntryWithLiveUntil>, HostError> {
        let _span = tracy_span!("storage get");
        Self::check_supported_ledger_key_type(key)?;
        self.prepare_read_only_access(key, budget)?;
        match self.map.get::<Rc<LedgerKey>>(key, budget)? {
            // Key has to be in the storage map at this point due to
            // `prepare_read_only_access`.
            None => Err((ScErrorType::Storage, ScErrorCode::InternalError).into()),
            Some(pair_option) => Ok(pair_option.clone()),
        }
    }

    /// Attempts to retrieve the [LedgerEntry] associated with a given
    /// [LedgerKey] in the [Storage], returning an error if the key is not
    /// found.
    ///
    /// In [FootprintMode::Recording] mode, records the read [LedgerKey] in the
    /// [Footprint] as [AccessType::ReadOnly] (unless already recorded as
    /// [AccessType::ReadWrite]) and reads through to the underlying
    /// [SnapshotSource], if the [LedgerKey] has not yet been loaded.
    ///
    /// In [FootprintMode::Enforcing] mode, succeeds only if the read
    /// [LedgerKey] has been declared in the [Footprint].
    pub fn get(
        &mut self,
        key: &Rc<LedgerKey>,
        budget: &Budget,
    ) -> Result<Rc<LedgerEntry>, HostError> {
        self.try_get(key, budget)?
            .ok_or_else(|| (ScErrorType::Storage, ScErrorCode::MissingValue).into())
    }

    // Like `get`, but distinguishes between missing values (return `Ok(None)`)
    // and out-of-footprint values or errors (`Err(...)`).
    pub(crate) fn try_get(
        &mut self,
        key: &Rc<LedgerKey>,
        budget: &Budget,
    ) -> Result<Option<Rc<LedgerEntry>>, HostError> {
        self.try_get_full(key, budget)
            .map(|ok| ok.map(|pair| pair.0))
    }

    /// Attempts to retrieve the [LedgerEntry] associated with a given
    /// [LedgerKey] and its live until ledger (if applicable) in the [Storage],
    /// returning an error if the key is not found.
    ///
    /// Live until ledgers only exist for `ContractData` and `ContractCode`
    /// ledger entries and are `None` for all the other entry kinds.
    ///
    /// In [FootprintMode::Recording] mode, records the read [LedgerKey] in the
    /// [Footprint] as [AccessType::ReadOnly] (unless already recorded as
    /// [AccessType::ReadWrite]) and reads through to the underlying
    /// [SnapshotSource], if the [LedgerKey] has not yet been loaded.
    ///
    /// In [FootprintMode::Enforcing] mode, succeeds only if the read
    /// [LedgerKey] has been declared in the [Footprint].
    pub(crate) fn get_with_live_until_ledger(
        &mut self,
        key: &Rc<LedgerKey>,
        budget: &Budget,
    ) -> Result<EntryWithLiveUntil, HostError> {
        self.try_get_full(key, budget)?
            .ok_or_else(|| (ScErrorType::Storage, ScErrorCode::MissingValue).into())
    }

    // Helper function `put` and `del` funnel into.
    fn put_opt(
        &mut self,
        key: &Rc<LedgerKey>,
        val: Option<EntryWithLiveUntil>,
        budget: &Budget,
    ) -> Result<(), HostError> {
        Self::check_supported_ledger_key_type(key)?;
        if let Some(le) = &val {
            Self::check_supported_ledger_entry_type(&le.0)?;
        }
        let ty = AccessType::ReadWrite;
        match self.mode {
            FootprintMode::Recording(_) => {
                self.footprint.record_access(key, ty, budget)?;
            }
            FootprintMode::Enforcing => {
                self.footprint.enforce_access(key, ty, budget)?;
            }
        };
        self.map = self.map.insert(Rc::clone(key), val, budget)?;
        Ok(())
    }

    /// Attempts to write to the [LedgerEntry] associated with a given
    /// [LedgerKey] in the [Storage].
    ///
    /// In [FootprintMode::Recording] mode, records the written [LedgerKey] in
    /// the [Footprint] as [AccessType::ReadWrite].
    ///
    /// In [FootprintMode::Enforcing] mode, succeeds only if the written
    /// [LedgerKey] has been declared in the [Footprint] as
    /// [AccessType::ReadWrite].
    pub fn put(
        &mut self,
        key: &Rc<LedgerKey>,
        val: &Rc<LedgerEntry>,
        live_until_ledger: Option<u32>,
        budget: &Budget,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("storage put");
        self.put_opt(key, Some((val.clone(), live_until_ledger)), budget)
    }

    /// Attempts to delete the [LedgerEntry] associated with a given [LedgerKey]
    /// in the [Storage].
    ///
    /// In [FootprintMode::Recording] mode, records the deleted [LedgerKey] in
    /// the [Footprint] as [AccessType::ReadWrite].
    ///
    /// In [FootprintMode::Enforcing] mode, succeeds only if the deleted
    /// [LedgerKey] has been declared in the [Footprint] as
    /// [AccessType::ReadWrite].
    pub fn del(&mut self, key: &Rc<LedgerKey>, budget: &Budget) -> Result<(), HostError> {
        let _span = tracy_span!("storage del");
        self.put_opt(key, None, budget)
    }

    /// Attempts to determine the presence of a [LedgerEntry] associated with a
    /// given [LedgerKey] in the [Storage], returning `Ok(true)` if an entry
    /// with the key exists and `Ok(false)` if it does not.
    ///
    /// In [FootprintMode::Recording] mode, records the access and reads-through
    /// to the underlying [SnapshotSource].
    ///
    /// In [FootprintMode::Enforcing] mode, succeeds only if the access has been
    /// declared in the [Footprint].
    pub fn has(&mut self, key: &Rc<LedgerKey>, budget: &Budget) -> Result<bool, HostError> {
        let _span = tracy_span!("storage has");
        Self::check_supported_ledger_key_type(key)?;
        self.prepare_read_only_access(key, budget)?;
        Ok(self
            .map
            .get::<Rc<LedgerKey>>(key, budget)?
            // Key has to be present in storage at this point, so not having it
            // would be an internal error.
            .ok_or_else(|| HostError::from((ScErrorType::Storage, ScErrorCode::InternalError)))?
            .is_some())
    }

    /// Extends `key` to live `extend_to` ledgers from now (not counting the
    /// current ledger) if the current live_until_ledger for the entry is
    /// `threshold` ledgers or less away from the current ledger.
    ///
    /// This operation is only defined within a host as it relies on ledger
    /// state.
    ///
    /// This operation does not modify any ledger entries, but does change the
    /// internal storage
    pub(crate) fn extend_ttl(
        &mut self,
        host: &Host,
        key: Rc<LedgerKey>,
        threshold: u32,
        extend_to: u32,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("extend key");
        Self::check_supported_ledger_key_type(&key)?;

        if threshold > extend_to {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InvalidInput,
                "threshold must be <= extend_to",
                &[threshold.into(), extend_to.into()],
            ));
        }

        // Extending deleted/non-existing/out-of-footprint entries will result in
        // an error.
        let (entry, old_live_until) = self.get_with_live_until_ledger(&key, host.budget_ref())?;
        let old_live_until = old_live_until.ok_or_else(|| {
            host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "trying to extend invalid entry",
                &[],
            )
        })?;

        let ledger_seq: u32 = host.get_ledger_sequence()?.into();
        if old_live_until < ledger_seq {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "accessing no-longer-live entry",
                &[old_live_until.into(), ledger_seq.into()],
            ));
        }

        let mut new_live_until = host.with_ledger_info(|li| {
            li.sequence_number.checked_add(extend_to).ok_or_else(|| {
                // overflowing here means a misconfiguration of the network (the
                // ttl is too large), in which case we immediately flag it as an
                // unrecoverable `InternalError`, even though the source is
                // external to the host.
                HostError::from(Error::from_type_and_code(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                ))
            })
        })?;

        if new_live_until > host.max_live_until_ledger()? {
            if let Some(durability) = get_key_durability(&key) {
                if matches!(durability, ContractDataDurability::Persistent) {
                    new_live_until = host.max_live_until_ledger()?;
                } else {
                    return Err(host.err(
                        ScErrorType::Storage,
                        ScErrorCode::InvalidAction,
                        "trying to extend past max live_until ledger",
                        &[new_live_until.into()],
                    ));
                }
            } else {
                return Err(host.err(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                    "durability is missing",
                    &[],
                ));
            }
        }

        if new_live_until > old_live_until && old_live_until.saturating_sub(ledger_seq) <= threshold
        {
            self.map = self.map.insert(
                key,
                Some((entry.clone(), Some(new_live_until))),
                host.budget_ref(),
            )?;
        }
        Ok(())
    }

    fn prepare_read_only_access(
        &mut self,
        key: &Rc<LedgerKey>,
        budget: &Budget,
    ) -> Result<(), HostError> {
        let ty = AccessType::ReadOnly;
        match self.mode {
            FootprintMode::Recording(ref src) => {
                self.footprint.record_access(key, ty, budget)?;
                // In recording mode we treat the map as a cache
                // that misses read-through to the underlying src.
                if !self.map.contains_key::<Rc<LedgerKey>>(key, budget)? {
                    #[cfg(any(test, feature = "unstable-next-api"))]
                    let value = src.get(&key)?;

                    #[cfg(not(any(test, feature = "unstable-next-api")))]
                    let value = if src.has(&key)? {
                        Some(src.get(key)?)
                    } else {
                        None
                    };

                    self.map = self.map.insert(key.clone(), value, budget)?;
                }
            }
            FootprintMode::Enforcing => {
                self.footprint.enforce_access(key, ty, budget)?;
            }
        };
        Ok(())
    }
}

#[cfg(any(test, feature = "testutils"))]
impl Storage {
    /// Unmetered function to access entry with lifetimes to test lifetime extensions.
    pub fn get_entry_with_lifetime(
        &self,
        key: Rc<LedgerKey>,
    ) -> Option<EntryWithLiveUntil> {
        let entry = self.map.map.iter().find(|e| e.0 == key);

        match entry {
            None => None,
            Some(pair_option) => pair_option.1.clone(),
        }
    }
}
