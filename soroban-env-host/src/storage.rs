//! This module contains the [Storage] type and its supporting types, which
//! provide the [Host](crate::Host) with access to durable ledger entries.
//!
//! For more details, see the [Env](crate::Env) data access functions:
//!   - [Env::has_contract_data](crate::Env::has_contract_data)
//!   - [Env::get_contract_data](crate::Env::get_contract_data)
//!   - [Env::put_contract_data](crate::Env::put_contract_data)
//!   - [Env::del_contract_data](crate::Env::del_contract_data)

use std::rc::Rc;

use soroban_env_common::xdr::{ScErrorCode, ScErrorType};
use soroban_env_common::{Env, Val};

use crate::budget::Budget;
use crate::xdr::{LedgerEntry, LedgerKey};
use crate::Host;
use crate::{host::metered_map::MeteredOrdMap, HostError};

pub type FootprintMap = MeteredOrdMap<Rc<LedgerKey>, AccessType, Budget>;
pub type StorageMap = MeteredOrdMap<Rc<LedgerKey>, Option<(Rc<LedgerEntry>, Option<u32>)>, Budget>;

/// The in-memory instance storage of the current running contract. Initially
/// contains entries from the `ScMap` of the corresponding `ScContractInstance`
/// contract data entry.
#[derive(Clone)]
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
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
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
pub trait SnapshotSource {
    // Returns the ledger entry for the key and its expiration.
    fn get(&self, key: &Rc<LedgerKey>) -> Result<(Rc<LedgerEntry>, Option<u32>), HostError>;
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
#[derive(Clone, Default)]
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
// covered by the underneath `MeteredOrdMap` and the `Footprint`'s own map.
impl Storage {
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
        let _span = tracy_span!("storage get");
        self.prepare_read_only_access(key, budget)?;
        match self.map.get::<Rc<LedgerKey>>(key, budget)? {
            None | Some(None) => Err((ScErrorType::Storage, ScErrorCode::MissingValue).into()),
            Some(Some((val, _))) => Ok(Rc::clone(val)),
        }
    }

    // Like `get`, but distinguishes between missing values (return `Ok(None)`)
    // and out-of-footprint values or errors (`Err(...)`).
    pub(crate) fn try_get(
        &mut self,
        key: &Rc<LedgerKey>,
        budget: &Budget,
    ) -> Result<Option<Rc<LedgerEntry>>, HostError> {
        let _span = tracy_span!("storage try_get");
        self.prepare_read_only_access(key, budget)?;
        match self.map.get::<Rc<LedgerKey>>(key, budget)? {
            // Key has to be in the storage map at this point due to
            // `prepare_read_only_access`.
            None => Err((ScErrorType::Storage, ScErrorCode::InternalError).into()),
            Some(None) => Ok(None),
            Some(Some((val, _))) => Ok(Some(Rc::clone(val))),
        }
    }

    /// Attempts to retrieve the [LedgerEntry] associated with a given
    /// [LedgerKey] and its expiration ledger (if applicable) in the [Storage],
    /// returning an error if the key is not found.
    ///
    /// Expiration ledgers only exist for `ContractData` and `ContractCode`
    /// ledger entries and are `None` for all the other entry kinds.
    ///
    /// In [FootprintMode::Recording] mode, records the read [LedgerKey] in the
    /// [Footprint] as [AccessType::ReadOnly] (unless already recorded as
    /// [AccessType::ReadWrite]) and reads through to the underlying
    /// [SnapshotSource], if the [LedgerKey] has not yet been loaded.
    ///
    /// In [FootprintMode::Enforcing] mode, succeeds only if the read
    /// [LedgerKey] has been declared in the [Footprint].
    pub(crate) fn get_with_expiration(
        &mut self,
        key: &Rc<LedgerKey>,
        budget: &Budget,
    ) -> Result<(Rc<LedgerEntry>, Option<u32>), HostError> {
        let _span = tracy_span!("storage get");
        self.prepare_read_only_access(key, budget)?;
        match self.map.get::<Rc<LedgerKey>>(key, budget)? {
            None | Some(None) => Err((ScErrorType::Storage, ScErrorCode::MissingValue).into()),
            Some(Some((val, expiration))) => Ok((Rc::clone(val), *expiration)),
        }
    }

    fn put_opt(
        &mut self,
        key: &Rc<LedgerKey>,
        val: Option<(&Rc<LedgerEntry>, Option<u32>)>,
        budget: &Budget,
    ) -> Result<(), HostError> {
        let ty = AccessType::ReadWrite;
        match self.mode {
            FootprintMode::Recording(_) => {
                self.footprint.record_access(key, ty, budget)?;
            }
            FootprintMode::Enforcing => {
                self.footprint.enforce_access(key, ty, budget)?;
            }
        };
        self.map = self.map.insert(
            Rc::clone(key),
            val.map(|(e, expiration)| (Rc::clone(e), expiration)),
            budget,
        )?;
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
        expiration_ledger: Option<u32>,
        budget: &Budget,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("storage put");
        self.put_opt(key, Some((val, expiration_ledger)), budget)
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
        self.prepare_read_only_access(key, budget)?;
        Ok(self
            .map
            .get::<Rc<LedgerKey>>(key, budget)?
            // Key has to be present in storage at this point, so not having it
            // would be an internal error.
            .ok_or_else(|| HostError::from((ScErrorType::Storage, ScErrorCode::InternalError)))?
            .is_some())
    }

    /// Bumps `key` to live for at least `bump_by_ledgers` from now (not
    /// counting the current ledger).
    ///
    /// This operation is only defined within a host as it relies on ledger
    /// state.
    ///
    /// This operation does not modify any ledger entries, but does change the
    /// internal storage
    pub fn bump(
        &mut self,
        host: &Host,
        key: Rc<LedgerKey>,
        threshold: u32,
        extend_to: u32,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("bump key");

        if threshold > extend_to {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InvalidInput,
                "threshold must be <= extend_to",
                &[threshold.into(), extend_to.into()],
            ));
        }

        // Bumping deleted/non-existing/out-of-footprint entries will result in
        // an error.
        let (entry, old_expiration) = self.get_with_expiration(&key, host.budget_ref())?;
        let old_expiration = old_expiration.ok_or_else(|| {
            host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "trying to bump non-expirable entry",
                &[],
            )
        })?;

        let ledger_seq: u32 = host.get_ledger_sequence()?.into();
        if old_expiration < ledger_seq {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "accessing expired entry",
                &[old_expiration.into(), ledger_seq.into()],
            ));
        }

        let new_expiration =
            host.with_ledger_info(|li| Ok(li.sequence_number.saturating_add(extend_to)))?;

        if new_expiration > host.max_expiration_ledger()? {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InvalidAction,
                "trying to bump past max expiration ledger",
                &[new_expiration.into()],
            ));
        }

        if new_expiration > old_expiration && old_expiration.saturating_sub(ledger_seq) <= threshold
        {
            self.map = self.map.insert(
                key,
                Some((entry.clone(), Some(new_expiration))),
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
