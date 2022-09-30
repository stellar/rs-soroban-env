use crate::host::{metered_clone::MeteredClone, Host};
use crate::native_contract::base_types::BigInt;
use crate::native_contract::token::error::Error;
use crate::native_contract::token::public_types::Identifier;
use crate::native_contract::token::storage_types::DataKey;
use soroban_env_common::{CheckedEnv, TryIntoVal};

// Metering: covered by components
pub fn read_nonce(e: &Host, id: Identifier) -> Result<BigInt, Error> {
    let key = DataKey::Nonce(id);
    if let Ok(nonce) = e.get_contract_data(key.try_into_val(e)?) {
        Ok(nonce.try_into_val(e)?)
    } else {
        Ok(BigInt::from_u64(e, 0)?)
    }
}

// Metering: covered by components
pub fn read_and_increment_nonce(e: &Host, id: Identifier) -> Result<BigInt, Error> {
    let key = DataKey::Nonce(id.metered_clone(&e.0.budget)?);
    let nonce = read_nonce(e, id)?;
    e.put_contract_data(
        key.try_into_val(e)?,
        (nonce.clone() + BigInt::from_u64(e, 1)?)?.try_into_val(e)?,
    )?;
    Ok(nonce)
}
