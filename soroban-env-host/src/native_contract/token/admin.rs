use crate::host::Host;
use crate::native_contract::base_types::Address;
use crate::native_contract::token::storage_types::DataKey;
use crate::HostError;
use soroban_env_common::{Env, StorageType, TryIntoVal};

// Metering: covered by components
pub fn read_administrator(e: &Host) -> Result<Address, HostError> {
    let key = DataKey::Admin;
    let rv = e.get_contract_data(key.try_into_val(e)?, StorageType::RECREATABLE)?;
    rv.try_into_val(e)
}

// Metering: covered by components
pub fn write_administrator(e: &Host, id: Address) -> Result<(), HostError> {
    let key = DataKey::Admin;
    e.put_contract_data(
        key.try_into_val(e)?,
        id.try_into_val(e)?,
        StorageType::RECREATABLE,
        ().into(),
    )?;
    Ok(())
}
