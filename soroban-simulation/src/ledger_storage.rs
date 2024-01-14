use super::state_ttl::{get_restored_ledger_sequence, TTLLedgerEntry};
use soroban_env_host::storage::SnapshotSource;
use soroban_env_host::xdr::ContractDataDurability::{Persistent, Temporary};
use soroban_env_host::xdr::{
    ConfigSettingEntry, ConfigSettingId, Error as XdrError, LedgerEntry, LedgerEntryData,
    LedgerKey, LedgerKeyConfigSetting, Limits, ScError, ScErrorCode, WriteXdr,
};
use soroban_env_host::HostError;
use std::cell::RefCell;
use std::collections::HashSet;
use std::convert::TryInto;
use std::ffi::NulError;
use std::rc::Rc;
use std::str::Utf8Error;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("not found")]
    NotFound,
    #[error("entry is not live")]
    NotLive,
    #[error("xdr processing error: {0}")]
    Xdr(#[from] XdrError),
    #[error("nul error: {0}")]
    NulError(#[from] NulError),
    #[error("utf8 error: {0}")]
    Utf8Error(#[from] Utf8Error),
    #[error("unexpected config ledger entry for setting_id {setting_id}")]
    UnexpectedConfigLedgerEntry { setting_id: String },
    #[error("unexpected ledger entry type ({ledger_entry_type}) for ttl ledger key")]
    UnexpectedLedgerEntryTypeForTtlKey { ledger_entry_type: String },
}

impl From<Error> for HostError {
    fn from(value: Error) -> Self {
        match value {
            Error::NotFound | Error::NotLive => ScError::Storage(ScErrorCode::MissingValue).into(),
            Error::Xdr(_) => ScError::Value(ScErrorCode::InvalidInput).into(),
            _ => ScError::Context(ScErrorCode::InternalError).into(),
        }
    }
}

struct EntryRestoreTracker {
    min_persistent_ttl: u32,
    // RefCell is needed to mutate the hashset inside SnapshotSource::get(), which is an immutable method
    ledger_keys_requiring_restore: RefCell<HashSet<LedgerKey>>,
}

impl EntryRestoreTracker {
    // Tracks ledger entries which need to be restored and returns its ttl as it was restored
    pub(crate) fn track_and_restore(
        &self,
        current_ledger_sequence: u32,
        key: &LedgerKey,
        entry_and_ttl: &(LedgerEntry, Option<u32>),
    ) -> Option<u32> {
        let ttl_entry: Box<dyn TTLLedgerEntry> = match entry_and_ttl.try_into() {
            Ok(e) => e,
            Err(_) => {
                // Nothing to track, the entry does not have a ttl
                return None;
            }
        };
        if ttl_entry.durability() != Persistent || ttl_entry.is_live(current_ledger_sequence) {
            // Nothing to track, the entry isn't persistent (and thus not restorable) or
            // it is alive
            return Some(ttl_entry.live_until_ledger_seq());
        }
        self.ledger_keys_requiring_restore
            .borrow_mut()
            .insert(key.clone());
        Some(get_restored_ledger_sequence(
            current_ledger_sequence,
            self.min_persistent_ttl,
        ))
    }
}

pub trait LedgerGetter {
    fn get(
        &self,
        key: &LedgerKey,
        include_not_live: bool,
    ) -> Result<(LedgerEntry, Option<u32>), Error>;
}

pub struct LedgerStorage {
    ledger_getter: Box<dyn LedgerGetter>,
    current_ledger_sequence: u32,
    restore_tracker: Option<EntryRestoreTracker>,
}

impl LedgerStorage {
    pub fn new(ledger_getter: Box<dyn LedgerGetter>, current_ledger_sequence: u32) -> Self {
        LedgerStorage {
            ledger_getter,
            current_ledger_sequence,
            restore_tracker: None,
        }
    }

    pub fn with_restore_tracking(
        ledger_getter: Box<dyn LedgerGetter>,
        current_ledger_sequence: u32,
    ) -> Result<Self, Error> {
        // First, we initialize it without the tracker, to get the minimum restore ledger from the network
        let mut ledger_storage = LedgerStorage {
            ledger_getter,
            current_ledger_sequence,
            restore_tracker: None,
        };
        let setting_id = ConfigSettingId::StateArchival;
        let ConfigSettingEntry::StateArchival(state_archival) =
            ledger_storage.get_configuration_setting(setting_id)?
        else {
            return Err(Error::UnexpectedConfigLedgerEntry {
                setting_id: setting_id.name().to_string(),
            });
        };
        // Now that we have the state archival config, we can build the tracker
        ledger_storage.restore_tracker = Some(EntryRestoreTracker {
            ledger_keys_requiring_restore: RefCell::new(HashSet::new()),
            min_persistent_ttl: state_archival.min_persistent_ttl,
        });
        Ok(ledger_storage)
    }

    pub(crate) fn get_xdr(
        &self,
        key: &LedgerKey,
        include_not_live: bool,
    ) -> Result<Vec<u8>, Error> {
        // TODO: this can be optimized since for entry types other than ContractCode/ContractData,
        //       they don't need to be deserialized and serialized again
        let (entry, _) = self.ledger_getter.get(key, include_not_live)?;
        Ok(entry.to_xdr(Limits::none())?)
    }

    pub(crate) fn get_configuration_setting(
        &self,
        setting_id: ConfigSettingId,
    ) -> Result<ConfigSettingEntry, Error> {
        let key = LedgerKey::ConfigSetting(LedgerKeyConfigSetting {
            config_setting_id: setting_id,
        });
        match self.ledger_getter.get(&key, false)? {
            (
                LedgerEntry {
                    data: LedgerEntryData::ConfigSetting(cs),
                    ..
                },
                _,
            ) => Ok(cs),
            _ => Err(Error::UnexpectedConfigLedgerEntry {
                setting_id: setting_id.name().to_string(),
            }),
        }
    }

    pub(crate) fn get_ledger_keys_requiring_restore(&self) -> HashSet<LedgerKey> {
        match self.restore_tracker {
            Some(ref t) => t.ledger_keys_requiring_restore.borrow().clone(),
            None => HashSet::new(),
        }
    }
}

impl LedgerGetter for LedgerStorage {
    fn get(
        &self,
        key: &LedgerKey,
        include_not_live: bool,
    ) -> anyhow::Result<(LedgerEntry, Option<u32>), Error> {
        self.ledger_getter.get(key, include_not_live)
    }
}

impl SnapshotSource for LedgerStorage {
    fn get(&self, key: &Rc<LedgerKey>) -> Result<(Rc<LedgerEntry>, Option<u32>), HostError> {
        if let Some(ref tracker) = self.restore_tracker {
            let mut entry_and_ttl = self.ledger_getter.get(key, true)?;
            // Explicitly discard temporary ttl'ed entries
            if let Ok(ttl_entry) = TryInto::<Box<dyn TTLLedgerEntry>>::try_into(&entry_and_ttl) {
                if ttl_entry.durability() == Temporary
                    && !ttl_entry.is_live(self.current_ledger_sequence)
                {
                    return Err(HostError::from(Error::NotLive));
                }
            }
            // If the entry is not live, we modify the ttl to make it seem like it was restored
            entry_and_ttl.1 =
                tracker.track_and_restore(self.current_ledger_sequence, key, &entry_and_ttl);
            return Ok((entry_and_ttl.0.into(), entry_and_ttl.1));
        }
        let entry_and_ttl = self
            .ledger_getter
            .get(key, false)
            .map_err(HostError::from)?;
        Ok((entry_and_ttl.0.into(), entry_and_ttl.1))
    }

    fn has(&self, key: &Rc<LedgerKey>) -> Result<bool, HostError> {
        let result = <dyn SnapshotSource>::get(self, key);
        if let Err(ref host_error) = result {
            if host_error.error.is_code(ScErrorCode::MissingValue) {
                return Ok(false);
            }
        }
        result.map(|_| true)
    }
}
