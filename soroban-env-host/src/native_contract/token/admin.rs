use crate::host::Host;
use crate::native_contract::token::error::Error;
use crate::native_contract::token::public_types::{Identifier, Signature};
use crate::native_contract::token::storage_types::DataKey;
use soroban_env_common::{CheckedEnv, TryFromVal, TryIntoVal};

fn read_administrator(e: &Host) -> Result<Identifier, Error> {
    let key = DataKey::Admin;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    Ok(Identifier::try_from_val(e, rv)?)
}

pub fn write_administrator(e: &Host, id: Identifier) -> Result<(), Error> {
    let key = DataKey::Admin;
    e.put_contract_data(key.try_into_val(e)?, id.try_into_val(e)?)?;
    Ok(())
}

pub fn check_admin(e: &Host, auth: &Signature) -> Result<(), Error> {
    let admin = read_administrator(e)?;
    let id = auth.get_identifier(e)?;

    if id != admin {
        Err(Error::ContractError)
    } else {
        Ok(())
    }
}
