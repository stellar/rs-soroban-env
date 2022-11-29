use crate::native_contract::token::public_types::Identifier;
use crate::native_contract::token::storage_types::DataKey;
use crate::{host::Host, HostError};
use soroban_env_common::{CheckedEnv, RawVal, TryIntoVal};

use super::error::ContractError;

// Metering: covered by components
pub fn read_nonce(e: &Host, id: Identifier) -> Result<i128, HostError> {
    let key = DataKey::Nonce(id);
    if let Ok(nonce) = e.get_contract_data(key.try_into_val(e)?) {
        Ok(nonce.try_into_val(e)?)
    } else {
        Ok(0)
    }
}

// Metering: covered by components
pub fn read_and_increment_nonce(e: &Host, id: Identifier) -> Result<i128, HostError> {
    let key = DataKey::Nonce(id.clone());
    let key_raw: RawVal = key.try_into_val(e)?;
    let old_nonce: i128 = read_nonce(e, id)?;
    let new_nonce = old_nonce
        .checked_add(1)
        .ok_or_else(|| e.err_status(ContractError::OverflowError))?;
    let new_nonce_raw: RawVal = new_nonce.try_into_val(e)?;
    e.put_contract_data(key_raw, new_nonce_raw)?;
    Ok(old_nonce)
}
