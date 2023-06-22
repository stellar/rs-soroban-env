use soroban_env_common::xdr::ContractDataDurability;

use crate::{Host, HostError, LedgerInfo};

impl Host {
    pub(crate) fn get_min_expiration_ledger(
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
        Ok(ledger_seq.saturating_add(min_expiration))
    }
}
