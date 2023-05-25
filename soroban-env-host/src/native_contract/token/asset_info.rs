use crate::native_contract::token::public_types::AssetInfo;
use crate::native_contract::token::storage_types::DataKey;
use crate::{host::Host, HostError};
use soroban_env_common::{Env, StorageType, TryIntoVal};

pub fn write_asset_info(e: &Host, asset_info: AssetInfo) -> Result<(), HostError> {
    let key = DataKey::AssetInfo;
    e.put_contract_data(
        key.try_into_val(e)?,
        asset_info.try_into_val(e)?,
        StorageType::RECREATABLE,
        ().into(),
    )?;
    Ok(())
}

pub fn read_asset_info(e: &Host) -> Result<AssetInfo, HostError> {
    let key = DataKey::AssetInfo;
    let rv = e.get_contract_data(key.try_into_val(e)?, StorageType::RECREATABLE)?;
    rv.try_into_val(e)
}

pub fn has_asset_info(e: &Host) -> Result<bool, HostError> {
    let key = DataKey::AssetInfo;
    let rv = e.has_contract_data(key.try_into_val(e)?, StorageType::RECREATABLE)?;
    Ok(rv.try_into()?)
}
