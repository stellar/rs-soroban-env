/// This module contains functionality to invoke host functions in embedder
/// environments using a clean host instance.
/// Also contains helpers for processing the ledger changes caused by these
/// host functions.
use std::{cmp::max, rc::Rc};

use crate::{
    budget::{AsBudget, Budget},
    events::Events,
    fees::LedgerEntryRentChange,
    host::{
        crypto::sha256_hash_from_bytes,
        ledger_info_helper::get_key_durability,
        metered_clone::{MeteredAlloc, MeteredClone, MeteredContainer, MeteredIterator},
        metered_xdr::{metered_from_xdr_with_budget, metered_write_xdr},
    },
    storage::{AccessType, Footprint, FootprintMap, SnapshotSource, Storage, StorageMap},
    xdr::{
        AccountId, ContractDataDurability, ContractEventType, DiagnosticEvent, HostFunction,
        LedgerEntry, LedgerEntryData, LedgerFootprint, LedgerKey, LedgerKeyAccount,
        LedgerKeyContractCode, LedgerKeyContractData, LedgerKeyTrustLine, ScErrorCode, ScErrorType,
        SorobanAuthorizationEntry, SorobanResources, TtlEntry,
    },
    DiagnosticLevel, Error, Host, HostError, LedgerInfo, MeteredOrdMap,
};

pub type TtlEntryMap = MeteredOrdMap<Rc<LedgerKey>, Rc<TtlEntry>, Budget>;

/// Result of invoking a single host function prepared for embedder consumption.
pub struct InvokeHostFunctionResult {
    /// Result value of the function, encoded `ScVal` XDR on success, or error.
    pub encoded_invoke_result: Result<Vec<u8>, HostError>,
    /// All the ledger changes caused by this invocation, including no-ops.
    /// This contains an entry for *every* item in the input footprint, even if
    /// it wasn't modified at all.
    ///
    /// Read-only entry can only have their live until ledger increased.
    /// Read-write entries can be modified arbitrarily or removed.
    ///
    /// Empty when invocation fails.
    pub ledger_changes: Vec<LedgerEntryChange>,
    /// All the events that contracts emitted during invocation, encoded as
    /// `ContractEvent` XDR.
    ///
    /// Empty when invocation fails.
    pub encoded_contract_events: Vec<Vec<u8>>,
}

/// Represents a change of the ledger entry from 'old' value to the 'new' one.
/// Only contains the final value of the entry (if any) and some minimal
/// information about the old entry for convenience.
#[derive(Default)]
pub struct LedgerEntryChange {
    /// Whether the ledger entry is read-only, as defined by the footprint.
    pub read_only: bool,

    /// Entry key encoded as `LedgerKey` XDR.
    pub encoded_key: Vec<u8>,
    /// Size of the old entry in bytes. This is size of `LedgerEntry` encoded
    /// XDR.
    pub old_entry_size_bytes: u32,
    /// New value of the ledger entry encoded as `LedgerEntry` XDR.
    /// Only set for non-removed, non-readonly values, otherwise `None`.
    pub encoded_new_value: Option<Vec<u8>>,
    /// Change of the live until state of the entry.
    /// Only set for entries that have a TTL, otherwise `None`.
    pub ttl_change: Option<LedgerEntryLiveUntilChange>,
}

/// Represents the live until-related state of the entry.
pub struct LedgerEntryLiveUntilChange {
    /// Hash of the LedgerKey for the entry that this live until ledger change is tied to
    pub key_hash: Vec<u8>,
    /// Durability of the entry.    
    pub durability: ContractDataDurability,
    /// Live until ledger of the old entry.
    pub old_live_until_ledger: u32,
    /// Live until ledger of the new entry. Guaranteed to always be greater than
    /// or equal to `old_live_until_ledger`.
    pub new_live_until_ledger: u32,
}

/// Returns the difference between the `storage` and its initial snapshot as
/// `LedgerEntryChanges`.
/// Returns an entry for every item in `storage` footprint.
pub fn get_ledger_changes<T: SnapshotSource>(
    budget: &Budget,
    storage: &Storage,
    init_storage_snapshot: &T,
    init_ttl_entries: TtlEntryMap,
) -> Result<Vec<LedgerEntryChange>, HostError> {
    // Skip allocation metering for this for the sake of simplicity - the
    // bounding factor here is XDR decoding which is metered.
    let mut changes = Vec::with_capacity(storage.map.len());

    let footprint_map = &storage.footprint.0;
    // We return any invariant errors here as internal errors, as they would
    // typically mean inconsistency between storage and snapshot that shouldn't
    // happen in embedder environments, or simply fundamental invariant bugs.
    let internal_error = || {
        HostError::from(Error::from_type_and_code(
            ScErrorType::Storage,
            ScErrorCode::InternalError,
        ))
    };
    for (key, entry_with_live_until_ledger) in storage.map.iter(budget)? {
        let mut entry_change = LedgerEntryChange::default();
        metered_write_xdr(budget, key.as_ref(), &mut entry_change.encoded_key)?;
        let durability = get_key_durability(key);

        if let Some(durability) = durability {
            let key_hash = match init_ttl_entries.get::<Rc<LedgerKey>>(key, budget)? {
                Some(ee) => ee.key_hash.0.to_vec(),
                None => sha256_hash_from_bytes(entry_change.encoded_key.as_slice(), budget)?,
            };

            entry_change.ttl_change = Some(LedgerEntryLiveUntilChange {
                key_hash,
                durability,
                old_live_until_ledger: 0,
                new_live_until_ledger: 0,
            });
        }
        if init_storage_snapshot.has(key)? {
            let (old_entry, old_live_until_ledger) = init_storage_snapshot.get(key)?;
            let mut buf = vec![];
            metered_write_xdr(budget, old_entry.as_ref(), &mut buf)?;
            entry_change.old_entry_size_bytes = buf.len() as u32;

            if let Some(ref mut ttl_change) = &mut entry_change.ttl_change {
                ttl_change.old_live_until_ledger =
                    old_live_until_ledger.ok_or_else(internal_error)?;
            }
        }
        if let Some((_, new_live_until_ledger)) = entry_with_live_until_ledger {
            if let Some(ref mut ttl_change) = &mut entry_change.ttl_change {
                // Never reduce the final live until ledger.
                ttl_change.new_live_until_ledger = max(
                    new_live_until_ledger.ok_or_else(internal_error)?,
                    ttl_change.old_live_until_ledger,
                );
            }
        }
        let maybe_access_type: Option<AccessType> =
            footprint_map.get::<Rc<LedgerKey>>(key, budget)?.copied();
        match maybe_access_type {
            Some(AccessType::ReadOnly) => {
                entry_change.read_only = true;
            }
            Some(AccessType::ReadWrite) => {
                if let Some((entry, _)) = entry_with_live_until_ledger {
                    let mut entry_buf = vec![];
                    metered_write_xdr(budget, entry.as_ref(), &mut entry_buf)?;
                    entry_change.encoded_new_value = Some(entry_buf);
                }
            }
            None => {
                return Err(internal_error());
            }
        }
        changes.push(entry_change);
    }
    Ok(changes)
}

/// Extracts the rent-related changes from the provided ledger changes.
///
/// Only meaningful changes are returned (i.e. no-op changes are skipped).
///
/// Extracted changes can be used to compute the rent fee via `fees::compute_rent_fee`.
pub fn extract_rent_changes(ledger_changes: &Vec<LedgerEntryChange>) -> Vec<LedgerEntryRentChange> {
    ledger_changes
        .iter()
        .filter_map(|entry_change| {
            // Rent changes are only relevant to non-removed entries with
            // a ttl.
            if let (Some(ttl_change), Some(encoded_new_value)) =
                (&entry_change.ttl_change, &entry_change.encoded_new_value)
            {
                // Skip the entry if 1. it is not extended and 2. the entry size has not increased
                if ttl_change.old_live_until_ledger >= ttl_change.new_live_until_ledger
                    && entry_change.old_entry_size_bytes >= encoded_new_value.len() as u32
                {
                    return None;
                }
                Some(LedgerEntryRentChange {
                    is_persistent: matches!(
                        ttl_change.durability,
                        ContractDataDurability::Persistent
                    ),
                    old_size_bytes: entry_change.old_entry_size_bytes,
                    new_size_bytes: encoded_new_value.len() as u32,
                    old_live_until_ledger: ttl_change.old_live_until_ledger,
                    new_live_until_ledger: ttl_change.new_live_until_ledger,
                })
            } else {
                None
            }
        })
        .collect()
}

/// Invokes a host function within a fresh host instance.
///
/// This collects the necessary inputs as encoded XDR and returns the outputs
/// as encoded XDR as well. This is supposed to encapsulate all the metered
/// operations needed to invoke a host function, including the input/output
/// decoding/encoding.
///
/// In order to get clean budget metering data, a clean budget has to be
/// provided as an input. It can then be examined immediately after execution in
/// order to get the precise metering data. Budget is not reset in case of
/// errors.
///
/// This may only fail when budget is exceeded or if there is an internal error.
/// Host function invocation errors are stored within
///  `Ok(InvokeHostFunctionResult)`.
///
/// When diagnostics are enabled, we try to populate `diagnostic_events`
/// even if the `InvokeHostFunctionResult` fails for any reason.
#[allow(clippy::too_many_arguments)]
pub fn invoke_host_function<T: AsRef<[u8]>, I: ExactSizeIterator<Item = T>>(
    budget: &Budget,
    enable_diagnostics: bool,
    encoded_host_fn: T,
    encoded_resources: T,
    encoded_source_account: T,
    encoded_auth_entries: I,
    ledger_info: LedgerInfo,
    encoded_ledger_entries: I,
    encoded_ttl_entries: I,
    base_prng_seed: T,
    diagnostic_events: &mut Vec<DiagnosticEvent>,
) -> Result<InvokeHostFunctionResult, HostError> {
    let _span0 = tracy_span!("invoke_host_function");

    let resources: SorobanResources =
        metered_from_xdr_with_budget(encoded_resources.as_ref(), &budget)?;
    let footprint = build_storage_footprint_from_xdr(&budget, resources.footprint)?;
    let storage_and_ttl_maps = build_storage_map_from_xdr_ledger_entries(
        &budget,
        &footprint,
        encoded_ledger_entries,
        encoded_ttl_entries,
        ledger_info.sequence_number,
    )?;

    let storage_map = storage_and_ttl_maps.0;
    let init_storage_map = storage_map.metered_clone(budget)?;

    let storage = Storage::with_enforcing_footprint_and_map(footprint, storage_map);
    let host = Host::with_storage_and_budget(storage, budget.clone());
    let auth_entries = host.build_auth_entries_from_xdr(encoded_auth_entries)?;
    let host_function: HostFunction = host.metered_from_xdr(encoded_host_fn.as_ref())?;
    let source_account: AccountId = host.metered_from_xdr(encoded_source_account.as_ref())?;
    host.set_source_account(source_account)?;
    host.set_ledger_info(ledger_info)?;
    host.set_authorization_entries(auth_entries)?;
    let seed32: [u8; 32] = base_prng_seed.as_ref().try_into().map_err(|_| {
        host.err(
            ScErrorType::Context,
            ScErrorCode::InternalError,
            "base PRNG seed is not 32-bytes long",
            &[],
        )
    })?;
    host.set_base_prng_seed(seed32)?;
    if enable_diagnostics {
        host.set_diagnostic_level(DiagnosticLevel::Debug)?;
    }
    let result = {
        let _span1 = tracy_span!("Host::invoke_function");
        host.invoke_function(host_function)
    };
    let (storage, events) = host.try_finish()?;
    if enable_diagnostics {
        extract_diagnostic_events(&events, diagnostic_events);
    }
    let encoded_invoke_result = result.map(|res| {
        let mut encoded_result_sc_val = vec![];
        metered_write_xdr(&budget, &res, &mut encoded_result_sc_val)?;
        Ok(encoded_result_sc_val)
    })?;
    if encoded_invoke_result.is_ok() {
        let init_storage_snapshot = StorageMapSnapshotSource {
            budget: &budget,
            map: &init_storage_map,
        };
        let ledger_changes = get_ledger_changes(
            &budget,
            &storage,
            &init_storage_snapshot,
            storage_and_ttl_maps.1,
        )?;
        let encoded_contract_events = encode_contract_events(budget, &events)?;
        Ok(InvokeHostFunctionResult {
            encoded_invoke_result,
            ledger_changes,
            encoded_contract_events,
        })
    } else {
        Ok(InvokeHostFunctionResult {
            encoded_invoke_result,
            ledger_changes: vec![],
            encoded_contract_events: vec![],
        })
    }
}

/// Encodes host events as `ContractEvent` XDR.
pub fn encode_contract_events(budget: &Budget, events: &Events) -> Result<Vec<Vec<u8>>, HostError> {
    let ce = events
        .0
        .iter()
        .filter(|e| !e.failed_call && e.event.type_ != ContractEventType::Diagnostic)
        .map(|e| {
            let mut buf = vec![];
            metered_write_xdr(budget, &e.event, &mut buf)?;
            Ok(buf)
        })
        .collect::<Result<Vec<Vec<u8>>, HostError>>()?;
    // Here we collect first then charge, so that the input size excludes the diagnostic events.
    // This means we may temporarily go over the budget limit but should be okay.
    Vec::<Vec<u8>>::charge_bulk_init_cpy(ce.len() as u64, budget)?;
    Ok(ce)
}

fn extract_diagnostic_events(events: &Events, diagnostic_events: &mut Vec<DiagnosticEvent>) {
    // Important: diagnostic events should be non-metered and not fallible in
    // order to not cause unitentional change in transaction result.
    for event in &events.0 {
        diagnostic_events.push(DiagnosticEvent {
            in_successful_contract_call: !event.failed_call,
            event: event.event.clone(),
        });
    }
}

fn ledger_entry_to_ledger_key(le: &LedgerEntry, budget: &Budget) -> Result<LedgerKey, HostError> {
    match &le.data {
        LedgerEntryData::Account(a) => Ok(LedgerKey::Account(LedgerKeyAccount {
            account_id: a.account_id.metered_clone(budget)?,
        })),
        LedgerEntryData::Trustline(tl) => Ok(LedgerKey::Trustline(LedgerKeyTrustLine {
            account_id: tl.account_id.metered_clone(budget)?,
            asset: tl.asset.metered_clone(budget)?,
        })),
        LedgerEntryData::ContractData(cd) => Ok(LedgerKey::ContractData(LedgerKeyContractData {
            contract: cd.contract.metered_clone(budget)?,
            key: cd.key.metered_clone(budget)?,
            durability: cd.durability,
        })),
        LedgerEntryData::ContractCode(code) => Ok(LedgerKey::ContractCode(LedgerKeyContractCode {
            hash: code.hash.metered_clone(budget)?,
        })),
        _ => {
            return Err(Error::from_type_and_code(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
            )
            .into());
        }
    }
}

fn build_storage_footprint_from_xdr(
    budget: &Budget,
    footprint: LedgerFootprint,
) -> Result<Footprint, HostError> {
    let mut footprint_map = FootprintMap::new();

    for key in footprint.read_write.as_vec() {
        Storage::check_supported_ledger_key_type(&key)?;
        footprint_map = footprint_map.insert(
            Rc::metered_new(key.metered_clone(budget)?, budget)?,
            AccessType::ReadWrite,
            budget,
        )?;
    }

    for key in footprint.read_only.as_vec() {
        Storage::check_supported_ledger_key_type(&key)?;
        footprint_map = footprint_map.insert(
            Rc::metered_new(key.metered_clone(budget)?, budget)?,
            AccessType::ReadOnly,
            budget,
        )?;
    }
    Ok(Footprint(footprint_map))
}

fn build_storage_map_from_xdr_ledger_entries<T: AsRef<[u8]>, I: ExactSizeIterator<Item = T>>(
    budget: &Budget,
    footprint: &Footprint,
    encoded_ledger_entries: I,
    encoded_ttl_entries: I,
    ledger_num: u32,
) -> Result<(StorageMap, TtlEntryMap), HostError> {
    let mut storage_map = StorageMap::new();
    let mut ttl_map = TtlEntryMap::new();

    if encoded_ledger_entries.len() != encoded_ttl_entries.len() {
        return Err(
            Error::from_type_and_code(ScErrorType::Storage, ScErrorCode::InternalError).into(),
        );
    }

    for (entry_buf, ttl_buf) in encoded_ledger_entries.zip(encoded_ttl_entries) {
        let mut live_until_ledger: Option<u32> = None;

        let le = Rc::metered_new(
            metered_from_xdr_with_budget::<LedgerEntry>(entry_buf.as_ref(), budget)?,
            budget,
        )?;
        let key = Rc::metered_new(ledger_entry_to_ledger_key(&le, budget)?, budget)?;

        if !ttl_buf.as_ref().is_empty() {
            let ee = Rc::metered_new(
                metered_from_xdr_with_budget::<TtlEntry>(ttl_buf.as_ref(), budget)?,
                budget,
            )?;

            if ee.live_until_ledger_seq < ledger_num {
                return Err(Error::from_type_and_code(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                )
                .into());
            }

            live_until_ledger = Some(ee.live_until_ledger_seq);

            ttl_map = ttl_map.insert(key.clone(), ee, budget)?;
        } else if matches!(le.as_ref().data, LedgerEntryData::ContractData(_))
            || matches!(le.as_ref().data, LedgerEntryData::ContractCode(_))
        {
            return Err(Error::from_type_and_code(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
            )
            .into());
        }

        if !footprint.0.contains_key::<LedgerKey>(&key, budget)? {
            return Err(Error::from_type_and_code(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
            )
            .into());
        }
        storage_map = storage_map.insert(key, Some((le, live_until_ledger)), budget)?;
    }

    // Add non-existing entries from the footprint to the storage.
    for k in footprint.0.keys(budget)? {
        if !storage_map.contains_key::<LedgerKey>(k, budget)? {
            storage_map = storage_map.insert(Rc::clone(k), None, budget)?;
        }
    }
    Ok((storage_map, ttl_map))
}

impl Host {
    fn build_auth_entries_from_xdr<T: AsRef<[u8]>, I: ExactSizeIterator<Item = T>>(
        &self,
        encoded_contract_auth_entries: I,
    ) -> Result<Vec<SorobanAuthorizationEntry>, HostError> {
        encoded_contract_auth_entries
            .map(|buf| self.metered_from_xdr::<SorobanAuthorizationEntry>(buf.as_ref()))
            .metered_collect::<Result<Vec<SorobanAuthorizationEntry>, HostError>>(
                self.as_budget(),
            )?
    }
}

struct StorageMapSnapshotSource<'a> {
    budget: &'a Budget,
    map: &'a StorageMap,
}

impl<'a> SnapshotSource for StorageMapSnapshotSource<'a> {
    fn get(&self, key: &Rc<LedgerKey>) -> Result<(Rc<LedgerEntry>, Option<u32>), HostError> {
        if let Some(Some((entry, live_until_ledger))) =
            self.map.get::<Rc<LedgerKey>>(key, self.budget)?
        {
            Ok((Rc::clone(entry), *live_until_ledger))
        } else {
            Err(Error::from_type_and_code(ScErrorType::Storage, ScErrorCode::InternalError).into())
        }
    }

    fn has(&self, key: &Rc<LedgerKey>) -> Result<bool, HostError> {
        if let Some(maybe_value) = self.map.get::<Rc<LedgerKey>>(key, self.budget)? {
            Ok(maybe_value.is_some())
        } else {
            Err(Error::from_type_and_code(ScErrorType::Storage, ScErrorCode::InternalError).into())
        }
    }
}
