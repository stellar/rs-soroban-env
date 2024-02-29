use crate::network_config::NetworkConfig;
use crate::simulation::{
    simulate_restore_op, RestoreOpSimulationResult, SimulationAdjustmentConfig,
};
use anyhow::{anyhow, Result};
use soroban_env_host::{
    ledger_info::get_key_durability,
    storage::{EntryWithLiveUntil, SnapshotSource},
    xdr::{ContractDataDurability, LedgerKey, ScErrorCode, ScErrorType},
    HostError, LedgerInfo,
};
use std::cell::RefCell;
use std::collections::BTreeSet;
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
