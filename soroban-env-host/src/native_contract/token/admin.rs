use crate::host::Host;
use crate::native_contract::base_types::Address;
use crate::native_contract::contract_error::ContractError;
use crate::native_contract::token::storage_types::DataKey;
use crate::{err, HostError};
use soroban_env_common::{Compare, Env, TryIntoVal};

// Metering: covered by components
fn read_administrator(e: &Host) -> Result<Address, HostError> {
    let key = DataKey::Admin;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    rv.try_into_val(e)
}

// Metering: covered by components
pub fn write_administrator(e: &Host, id: Address) -> Result<(), HostError> {
    let key = DataKey::Admin;
    e.put_contract_data(key.try_into_val(e)?, id.try_into_val(e)?)?;
    Ok(())
}

// Metering: *mostly* covered by components.
pub fn check_admin(e: &Host, addr: &Address) -> Result<(), HostError> {
    let admin = read_administrator(e)?;
    if e.compare(&admin, addr)? == core::cmp::Ordering::Equal {
        Ok(())
    } else {
        Err(err!(
            e,
            ContractError::UnauthorizedError,
            "address '{}' is not an admin ('{}')",
            addr.clone(),
            admin
        ))
    }
}
