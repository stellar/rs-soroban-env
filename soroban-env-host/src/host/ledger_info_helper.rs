use soroban_env_common::xdr::{ContractDataDurability, LedgerKey};

use crate::{Host, HostError, LedgerInfo};

impl Host {
    pub(crate) fn get_min_live_until_ledger(
        &self,
        storage_type: ContractDataDurability,
    ) -> Result<u32, HostError> {
        let ledger_seq = self.with_ledger_info(|li| Ok(li.sequence_number))?;
        let min_expiration = match storage_type {
            ContractDataDurability::Temporary => {
                self.with_ledger_info(|li| Ok(li.min_temp_entry_expiration))?
            }
            ContractDataDurability::Persistent => {
                self.with_ledger_info(|li: &LedgerInfo| Ok(li.min_persistent_entry_expiration))?
            }
        };
        Ok(ledger_seq.saturating_add(min_expiration.saturating_sub(1)))
    }

    pub(crate) fn max_live_until_ledger(&self) -> Result<u32, HostError> {
        self.with_ledger_info(|li| {
            Ok(li
                .sequence_number
                // Entry can live for at most max_entry_expiration ledgers from
                // now, counting the current one.
                .saturating_add(li.max_entry_expiration.saturating_sub(1)))
        })
    }
}

pub fn get_key_durability(key: &LedgerKey) -> Option<ContractDataDurability> {
    match &key {
        LedgerKey::ContractData(d) => Some(d.durability),
        LedgerKey::ContractCode(_) => Some(ContractDataDurability::Persistent),
        _ => None,
    }
}
