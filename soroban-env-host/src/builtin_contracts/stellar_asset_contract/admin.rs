use super::storage_types::InstanceDataKey;
use crate::{
    builtin_contracts::base_types::Address, host::Host, Env, HostError, StorageType, TryIntoVal,
};

// Metering: covered by components
pub(crate) fn read_administrator(e: &Host) -> Result<Address, HostError> {
    let key = InstanceDataKey::Admin;
    let rv = e.get_contract_data(key.try_into_val(e)?, StorageType::Instance)?;
    rv.try_into_val(e)
}

// Metering: covered by components
pub(crate) fn write_administrator(e: &Host, id: Address) -> Result<(), HostError> {
    let key = InstanceDataKey::Admin;
    e.put_contract_data(
        key.try_into_val(e)?,
        id.try_into_val(e)?,
        StorageType::Instance,
    )?;
    Ok(())
}
