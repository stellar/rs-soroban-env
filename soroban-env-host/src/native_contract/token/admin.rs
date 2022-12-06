use crate::host::Host;
use crate::native_contract::token::public_types::{Identifier, Signature};
use crate::native_contract::token::storage_types::DataKey;
use crate::{err, HostError};
use soroban_env_common::{CheckedEnv, Compare, TryFromVal, TryIntoVal};

use super::error::ContractError;

// Metering: covered by components
fn read_administrator(e: &Host) -> Result<Identifier, HostError> {
    let key = DataKey::Admin;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    Ok(Identifier::try_from_val(e, rv)?)
}

// Metering: covered by components
pub fn write_administrator(e: &Host, id: Identifier) -> Result<(), HostError> {
    let key = DataKey::Admin;
    e.put_contract_data(key.try_into_val(e)?, id.try_into_val(e)?)?;
    Ok(())
}

// Metering: *mostly* covered by components.
pub fn check_admin(e: &Host, auth: &Signature) -> Result<(), HostError> {
    let admin = read_administrator(e)?;
    let id = auth.get_identifier(e)?;

    if e.compare(&id, &admin)? != core::cmp::Ordering::Equal {
        Err(err!(
            e,
            ContractError::UnauthorizedError,
            "identifer '{}' is not an admin ('{}')",
            id,
            admin
        ))
    } else {
        Ok(())
    }
}
