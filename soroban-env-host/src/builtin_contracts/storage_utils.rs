use crate::{Env, Host, HostError, StorageType, Val};

impl Host {
    pub(crate) fn try_get_contract_data(
        &self,
        k: Val,
        t: StorageType,
    ) -> Result<Option<Val>, HostError> {
        if self.has_contract_data(k, t)?.into() {
            Ok(Some(self.get_contract_data(k, t)?))
        } else {
            Ok(None)
        }
    }
}
