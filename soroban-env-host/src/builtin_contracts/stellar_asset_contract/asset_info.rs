use crate::builtin_contracts::stellar_asset_contract::public_types::AssetInfo;
use crate::builtin_contracts::stellar_asset_contract::storage_types::InstanceDataKey;
use crate::{host::Host, HostError};
use soroban_env_common::{Env, StorageType, TryIntoVal};

pub fn write_asset_info(e: &Host, asset_info: AssetInfo) -> Result<(), HostError> {
    let key = InstanceDataKey::AssetInfo;
    e.put_contract_data(
        key.try_into_val(e)?,
        asset_info.try_into_val(e)?,
        StorageType::Instance,
    )?;
    Ok(())
}

pub fn read_asset_info(e: &Host) -> Result<AssetInfo, HostError> {
    let key = InstanceDataKey::AssetInfo;
    let rv = e.get_contract_data(key.try_into_val(e)?, StorageType::Instance)?;
    rv.try_into_val(e)
}

pub fn has_asset_info(e: &Host) -> Result<bool, HostError> {
    let key = InstanceDataKey::AssetInfo;
    let rv = e.has_contract_data(key.try_into_val(e)?, StorageType::Instance)?;
    Ok(rv.try_into()?)
}
