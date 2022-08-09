use crate::host::Host;
use crate::native_contract::base_types::BigInt;
use crate::native_contract::token::error::Error;
use crate::native_contract::token::public_types::Identifier;
use crate::native_contract::token::storage_types::DataKey;
use soroban_env_common::{CheckedEnv, TryIntoVal};

pub fn read_nonce(e: &Host, id: Identifier) -> Result<BigInt, Error> {
    let key = DataKey::Nonce(id);
    if let Ok(nonce) = e.get_contract_data(key.try_into_val(e)?) {
        Ok(nonce.in_env(e).try_into()?)
    } else {
        Ok(BigInt::from_u64(e, 0)?)
    }
}

pub fn read_and_increment_nonce(e: &Host, id: Identifier) -> Result<BigInt, Error> {
    let key = DataKey::Nonce(id.clone());
    let nonce = read_nonce(e, id)?;
    e.put_contract_data(
        key.try_into_val(e)?,
        (nonce.clone() + BigInt::from_u64(e, 1)?)?.try_into_val(e)?,
    )?;
    Ok(nonce)
}
