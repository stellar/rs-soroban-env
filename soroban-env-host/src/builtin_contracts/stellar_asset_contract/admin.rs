use crate::builtin_contracts::base_types::Address;
use crate::host::Host;
use crate::HostError;
use soroban_env_common::{Env, StorageType, TryIntoVal};

use super::storage_types::InstanceDataKey;

// Metering: covered by components
pub fn read_administrator(e: &Host) -> Result<Address, HostError> {
    let key = InstanceDataKey::Admin;
    let rv = e.get_contract_data(key.try_into_val(e)?, StorageType::Instance)?;
    rv.try_into_val(e)
}

// Metering: covered by components
pub fn write_administrator(e: &Host, id: Address) -> Result<(), HostError> {
    let key = InstanceDataKey::Admin;
    e.put_contract_data(
        key.try_into_val(e)?,
        id.try_into_val(e)?,
        StorageType::Instance,
    )?;
    Ok(())
}
