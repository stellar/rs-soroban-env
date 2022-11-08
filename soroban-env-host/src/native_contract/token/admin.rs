use crate::host::Host;
use crate::native_contract::contract_error::ContractError;
use crate::native_contract::token::storage_types::DataKey;
use crate::{err, HostError};
use soroban_env_common::xdr::ScAddress;
use soroban_env_common::{CheckedEnv, TryFromVal, TryIntoVal};

// Metering: covered by components
fn read_administrator(e: &Host) -> Result<ScAddress, HostError> {
    let key = DataKey::Admin;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    Ok(ScAddress::try_from_val(e, rv)?)
}

// Metering: covered by components
pub fn write_administrator(e: &Host, id: ScAddress) -> Result<(), HostError> {
    let key = DataKey::Admin;
    e.put_contract_data(key.try_into_val(e)?, id.try_into_val(e)?)?;
    Ok(())
}

// Metering: *mostly* covered by components.
pub fn check_admin(e: &Host, addr: &ScAddress) -> Result<(), HostError> {
    let admin = read_administrator(e)?;
    if addr != &admin {
        Err(err!(
            e,
            ContractError::UnauthorizedError,
            "address '{}' is not an admin ('{}')",
            addr.clone(),
            admin
        ))
    } else {
        Ok(())
    }
}
