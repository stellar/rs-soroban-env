use soroban_env_common::{Env, StorageType, Val};

use crate::{Host, HostError};

pub(crate) struct StorageUtils {}

impl StorageUtils {
    pub(crate) fn try_get(e: &Host, k: Val, t: StorageType) -> Result<Option<Val>, HostError> {
        if e.has_contract_data(k, t)?.into() {
            Ok(Some(e.get_contract_data(k, t)?))
        } else {
            Ok(None)
        }
    }
}
