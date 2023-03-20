use crate::native_contract::base_types::Bytes;
use crate::native_contract::token::public_types::Metadata;
use crate::native_contract::token::storage_types::DataKey;
use crate::{host::Host, HostError};
use soroban_env_common::{Env, EnvBase, TryFromVal, TryIntoVal};

// Metering: *mostly* covered by components.
pub fn write_metadata(e: &Host, metadata: Metadata) -> Result<(), HostError> {
    let key = DataKey::Metadata;
    e.put_contract_data(key.try_into_val(e)?, metadata.try_into_val(e)?)?;
    Ok(())
}

// Metering: *mostly* covered by components.
pub fn read_metadata(e: &Host) -> Result<Metadata, HostError> {
    let key = DataKey::Metadata;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    rv.try_into_val(e)
}

// Metering: *mostly* covered by components.
pub fn has_metadata(e: &Host) -> Result<bool, HostError> {
    let key = DataKey::Metadata;
    let rv = e.has_contract_data(key.try_into_val(e)?)?;
    Ok(rv.try_into()?)
}

// Metering: *mostly* covered by components. `bytes_new_from_slice` and `Bytes` not covered.
pub fn read_name(e: &Host) -> Result<Bytes, HostError> {
    match read_metadata(e)? {
        Metadata::Native => Ok(Bytes::try_from_val(e, &e.bytes_new_from_slice(b"native")?)?),
        Metadata::AlphaNum4(asset) => {
            let mut res: Bytes = asset.asset_code.into();
            res.push(b':')?;
            res.append(asset.issuer.into())?;
            Ok(res)
        }
        Metadata::AlphaNum12(asset) => {
            let mut res: Bytes = asset.asset_code.into();
            res.push(b':')?;
            res.append(asset.issuer.into())?;
            Ok(res)
        }
    }
}

// Metering: *mostly* covered by components.`bytes_new_from_slice` and `Bytes` not covered.
pub fn read_symbol(e: &Host) -> Result<Bytes, HostError> {
    match read_metadata(e)? {
        Metadata::Native => Ok(Bytes::try_from_val(e, &e.bytes_new_from_slice(b"native")?)?),
        Metadata::AlphaNum4(asset) => Ok(asset.asset_code.into()),
        Metadata::AlphaNum12(asset) => Ok(asset.asset_code.into()),
    }
}
