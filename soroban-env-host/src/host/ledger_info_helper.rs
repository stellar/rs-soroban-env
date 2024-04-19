use crate::{
    xdr::{ContractDataDurability, ScErrorCode, ScErrorType},
    Error, Host, HostError,
};

impl Host {
    pub(crate) fn get_min_live_until_ledger(
        &self,
        durability: ContractDataDurability,
    ) -> Result<u32, HostError> {
        self.with_ledger_info(|li| {
            li.min_live_until_ledger_checked(durability).ok_or_else(|| {
                // overflowing here means a misconfiguration of the network (the
                // ttl is too large), in which case we immediately flag it as an
                // unrecoverable `InternalError`, even though the source is
                // external to the host.
                HostError::from(Error::from_type_and_code(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                ))
            })
        })
    }

    pub(crate) fn max_live_until_ledger(&self) -> Result<u32, HostError> {
        self.with_ledger_info(|li| {
            li.max_live_until_ledger_checked().ok_or_else(|| {
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
