use crate::{
    xdr::{ContractDataDurability, LedgerKey, ScErrorCode, ScErrorType},
    Error, Host, HostError, LedgerInfo,
};

impl Host {
    pub(crate) fn get_min_live_until_ledger(
        &self,
        storage_type: ContractDataDurability,
    ) -> Result<u32, HostError> {
        let ledger_seq = self.with_ledger_info(|li| Ok(li.sequence_number))?;
        let min_live_until = match storage_type {
            ContractDataDurability::Temporary => {
                self.with_ledger_info(|li| Ok(li.min_temp_entry_ttl))?
            }
            ContractDataDurability::Persistent => {
                self.with_ledger_info(|li: &LedgerInfo| Ok(li.min_persistent_entry_ttl))?
            }
        };
        ledger_seq
            .checked_add(min_live_until.saturating_sub(1))
            .ok_or_else(|| {
                // overflowing here means a misconfiguration of the network (the
                // ttl is too large), in which case we immediately flag it as an
                // unrecoverable `InternalError`, even though the source is
                // external to the host.
                HostError::from(Error::from_type_and_code(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                ))
            })
    }

    pub(crate) fn max_live_until_ledger(&self) -> Result<u32, HostError> {
        self.with_ledger_info(|li| {
            li.sequence_number
                // Entry can live for at most max_entry_live_until ledgers from
                // now, counting the current one.
                .checked_add(li.max_entry_ttl.saturating_sub(1))
                .ok_or_else(|| {
                    // overflowing here means a misconfiguration of the network
                    // (the ttl is too large), in which case we immediately flag
                    // it as an unrecoverable `InternalError`, even though the
                    // source is external to the host.
                    HostError::from(Error::from_type_and_code(
                        ScErrorType::Context,
                        ScErrorCode::InternalError,
                    ))
                })
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
