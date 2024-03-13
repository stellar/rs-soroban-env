use crate::network_config::NetworkConfig;
use crate::simulation::{
    simulate_restore_op, RestoreOpSimulationResult, SimulationAdjustmentConfig,
};
use anyhow::{anyhow, Result};
use soroban_env_host::xdr::{
    AccountEntry, AccountEntryExt, AccountEntryExtensionV1, AccountEntryExtensionV1Ext,
    AccountEntryExtensionV2, AccountEntryExtensionV2Ext, AccountEntryExtensionV3, ExtensionPoint,
    LedgerEntryData, Liabilities, SponsorshipDescriptor, TimePoint,
};
use soroban_env_host::{
    ledger_info::get_key_durability,
    storage::{EntryWithLiveUntil, SnapshotSource},
    xdr::{ContractDataDurability, LedgerKey, ScErrorCode, ScErrorType},
    HostError, LedgerInfo,
};
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

/// Read-only ledger snapshot accessor that also has access
/// to archived entries.
///
/// Unlike `SnapshotSource` trait that must only return live entries,
/// this should return both live and archived ledger entries.
pub trait SnapshotSourceWithArchive {
    fn get_including_archived(
        &self,
        key: &Rc<LedgerKey>,
    ) -> std::result::Result<Option<EntryWithLiveUntil>, HostError>;
}

/// The `SnapshotSource` implementation that automatically restores
/// the archived ledger entries accessed during `get` calls.
///
/// This does not define a concrete implementation for ledger lookups
/// and just wraps a `SnapshotSourceWithArchive`.
///
/// The restored entries will have the rent automatically bumped to
/// `min_persistent_entry_ttl`, which is consistent with the behavior
/// of `RestoreFootprintOp` if it was called in the same ledger.
pub struct AutoRestoringSnapshotSource<T: SnapshotSourceWithArchive> {
    snapshot_source: Rc<T>,
    min_persistent_live_until_ledger: u32,
    current_ledger_sequence: u32,
    restored_ledger_keys: RefCell<BTreeSet<Rc<LedgerKey>>>,
}

impl<T: SnapshotSourceWithArchive> AutoRestoringSnapshotSource<T> {
    pub fn new(snapshot_source: Rc<T>, ledger_info: &LedgerInfo) -> Result<Self, anyhow::Error> {
        Ok(Self {
            snapshot_source,
            min_persistent_live_until_ledger: ledger_info.min_live_until_ledger_checked(ContractDataDurability::Persistent).ok_or_else(|| anyhow!("minimum persistent live until ledger overflows - ledger info is misconfigured"))?,
            current_ledger_sequence: ledger_info.sequence_number,
            restored_ledger_keys: RefCell::new(Default::default()),
        })
    }

    /// Resets all the restored keys recorded so far.
    pub fn reset_restored_keys(&self) {
        self.restored_ledger_keys.borrow_mut().clear();
    }

    /// Simulates a `RestoreFootprintOp` for all the keys that have been
    /// restored so far.
    pub fn simulate_restore_keys_op(
        &self,
        network_config: &NetworkConfig,
        adjustment_config: &SimulationAdjustmentConfig,
        ledger_info: &LedgerInfo,
    ) -> Result<Option<RestoreOpSimulationResult>> {
        let restored_keys = self.restored_ledger_keys.borrow();
        if restored_keys.is_empty() {
            return Ok(None);
        }
        simulate_restore_op(
            self.snapshot_source.as_ref(),
            network_config,
            adjustment_config,
            ledger_info,
            restored_keys
                .iter()
                .map(|k| k.as_ref().clone())
                .collect::<Vec<LedgerKey>>()
                .as_ref(),
        )
        .map(|res| Some(res))
    }
}

impl<T: SnapshotSourceWithArchive> SnapshotSource for AutoRestoringSnapshotSource<T> {
    fn get(&self, key: &Rc<LedgerKey>) -> Result<Option<EntryWithLiveUntil>, HostError> {
        let entry_with_live_until = self.snapshot_source.get_including_archived(key)?;
        if let Some((entry, live_until)) = entry_with_live_until {
            if let Some(durability) = get_key_durability(key.as_ref()) {
                let live_until = live_until.ok_or_else(|| {
                    // Entries with durability must have TTL.
                    HostError::from((ScErrorType::Storage, ScErrorCode::InternalError))
                })?;
                if live_until < self.current_ledger_sequence {
                    return match durability {
                        ContractDataDurability::Temporary => Ok(None),
                        ContractDataDurability::Persistent => {
                            let mut restored_ledger_keys =
                                self.restored_ledger_keys.try_borrow_mut().map_err(|_| {
                                    HostError::from((
                                        ScErrorType::Context,
                                        ScErrorCode::InternalError,
                                    ))
                                })?;
                            restored_ledger_keys.insert(key.clone());
                            Ok(Some((entry, Some(self.min_persistent_live_until_ledger))))
                        }
                    };
                }
            }
            Ok(Some((entry, live_until)))
        } else {
            Ok(None)
        }
    }
}

#[derive(Default)]
struct LedgerEntryUpdater {
    updated_entries_cache: BTreeMap<Rc<LedgerKey>, Option<EntryWithLiveUntil>>,
}

impl LedgerEntryUpdater {
    fn maybe_update_entry(
        &mut self,
        key: &Rc<LedgerKey>,
        entry: Option<EntryWithLiveUntil>,
    ) -> Option<EntryWithLiveUntil> {
        if let Some(e) = self.updated_entries_cache.get(key) {
            return e.clone();
        }
        if let Some((entry, live_until)) = &entry {
            match &entry.data {
                LedgerEntryData::Account(_) => {
                    let mut updated_entry = (**entry).clone();
                    match &mut updated_entry.data {
                        LedgerEntryData::Account(acc) => {
                            update_account_entry(acc);
                        }
                        _ => (),
                    }
                    let entry_with_live_until = Some((Rc::new(updated_entry), *live_until));
                    self.updated_entries_cache
                        .insert(key.clone(), entry_with_live_until.clone());
                    return entry_with_live_until;
                }
                _ => (),
            }
        }
        entry
    }
}

enum SnapshotSourceHolder<'a> {
    Ref(&'a dyn SnapshotSource),
    Rc(Rc<dyn SnapshotSource>),
}

impl<'a> SnapshotSource for SnapshotSourceHolder<'a> {
    fn get(&self, key: &Rc<LedgerKey>) -> Result<Option<EntryWithLiveUntil>, HostError> {
        match self {
            SnapshotSourceHolder::Ref(r) => r.get(key),
            SnapshotSourceHolder::Rc(r) => r.get(key),
        }
    }
}

// This is an internal wrapper for the snapshot sources used in the simulation.
// The purpose of the wrapper is to emulate any ledger entry modifying logic
// that Core might perform before the Soroban host operation is invoked. For example,
// Core might create the account extensions, which would impact the read bytes
// amount and simulated CPU instructions.
pub(crate) struct SimulationSnapshotSource<'a> {
    inner_snapshot: SnapshotSourceHolder<'a>,
    entry_updater: RefCell<LedgerEntryUpdater>,
}

// This is the same as `SimulationSnapshotSource`, but for
// `SnapshotSourceWithArchive` trait.
// Note, that we don't implement both traits for `SimulationSnapshotSource` in order
// to avoid confusion: we never want to accidentally create a `SnapshotSource` with
// `SnapshotSourceWithArchive` inner snapshot, or vice versa.
pub(crate) struct SimulationSnapshotSourceWithArchive<'a, T: SnapshotSourceWithArchive> {
    inner_snapshot: &'a T,
    entry_updater: RefCell<LedgerEntryUpdater>,
}

impl<'a> SimulationSnapshotSource<'a> {
    pub(crate) fn new(snapshot: &'a dyn SnapshotSource) -> Self {
        Self {
            inner_snapshot: SnapshotSourceHolder::Ref(snapshot),
            entry_updater: RefCell::new(Default::default()),
        }
    }

    pub(crate) fn new_from_rc(snapshot: Rc<dyn SnapshotSource>) -> Self {
        Self {
            inner_snapshot: SnapshotSourceHolder::Rc(snapshot),
            entry_updater: RefCell::new(Default::default()),
        }
    }
}

impl<'a, T: SnapshotSourceWithArchive> SimulationSnapshotSourceWithArchive<'a, T> {
    pub(crate) fn new(snapshot: &'a T) -> Self {
        Self {
            inner_snapshot: &snapshot,
            entry_updater: RefCell::new(Default::default()),
        }
    }
}

impl<'a> SnapshotSource for SimulationSnapshotSource<'a> {
    fn get(&self, key: &Rc<LedgerKey>) -> Result<Option<EntryWithLiveUntil>, HostError> {
        Ok(self
            .entry_updater
            .borrow_mut()
            .maybe_update_entry(key, self.inner_snapshot.get(key)?))
    }
}

impl<'a, T: SnapshotSourceWithArchive> SnapshotSourceWithArchive
    for SimulationSnapshotSourceWithArchive<'a, T>
{
    fn get_including_archived(
        &self,
        key: &Rc<LedgerKey>,
    ) -> Result<Option<EntryWithLiveUntil>, HostError> {
        Ok(self
            .entry_updater
            .borrow_mut()
            .maybe_update_entry(key, self.inner_snapshot.get_including_archived(key)?))
    }
}

fn update_account_entry(account_entry: &mut AccountEntry) {
    match &mut account_entry.ext {
        AccountEntryExt::V0 => {
            let mut ext = AccountEntryExtensionV1 {
                liabilities: Liabilities {
                    buying: 0,
                    selling: 0,
                },
                ext: AccountEntryExtensionV1Ext::V0,
            };
            fill_account_ext_v2(&mut ext, account_entry.signers.len());
            account_entry.ext = AccountEntryExt::V1(ext);
        }
        AccountEntryExt::V1(ext) => {
            fill_account_ext_v2(ext, account_entry.signers.len());
        }
    }
}

fn fill_account_ext_v2(account_ext_v1: &mut AccountEntryExtensionV1, signers_count: usize) {
    match &mut account_ext_v1.ext {
        AccountEntryExtensionV1Ext::V0 => {
            let mut ext = AccountEntryExtensionV2 {
                num_sponsored: 0,
                num_sponsoring: 0,
                signer_sponsoring_i_ds: vec![SponsorshipDescriptor(None); signers_count]
                    .try_into()
                    .unwrap_or_default(),
                ext: AccountEntryExtensionV2Ext::V0,
            };
            fill_account_ext_v3(&mut ext);
            account_ext_v1.ext = AccountEntryExtensionV1Ext::V2(ext);
        }
        AccountEntryExtensionV1Ext::V2(ext) => fill_account_ext_v3(ext),
    }
}

fn fill_account_ext_v3(account_ext_v2: &mut AccountEntryExtensionV2) {
    match account_ext_v2.ext {
        AccountEntryExtensionV2Ext::V0 => {
            account_ext_v2.ext = AccountEntryExtensionV2Ext::V3(AccountEntryExtensionV3 {
                ext: ExtensionPoint::V0,
                seq_ledger: 0,
                seq_time: TimePoint(0),
            });
        }
        AccountEntryExtensionV2Ext::V3(_) => (),
    }
}
