use crate::native_contract::base_types::Bytes;
use crate::native_contract::token::public_types::Metadata;
use crate::native_contract::token::storage_types::DataKey;
use crate::{host::Host, HostError};
use soroban_env_common::{CheckedEnv, Convert, EnvBase};

// Metering: *mostly* covered by components.
pub fn write_metadata(e: &Host, metadata: Metadata) -> Result<(), HostError> {
    let key = DataKey::Metadata;
    e.put_contract_data(e.convert(key)?, e.convert(metadata)?)?;
    Ok(())
}

// Metering: *mostly* covered by components.
pub fn read_metadata(e: &Host) -> Result<Metadata, HostError> {
    let key = DataKey::Metadata;
    let rv = e.get_contract_data(e.convert(key)?)?;
    Ok(e.convert(rv)?)
}

// Metering: *mostly* covered by components.
pub fn has_metadata(e: &Host) -> Result<bool, HostError> {
    let key = DataKey::Metadata;
    let rv = e.has_contract_data(e.convert(key)?)?;
    Ok(rv.try_into()?)
}

// Metering: *mostly* covered by components. `bytes_new_from_slice` and `Bytes` not covered.
pub fn read_name(e: &Host) -> Result<Bytes, HostError> {
    match read_metadata(e)? {
        Metadata::Token(token) => Ok(token.name),
        Metadata::Native => Ok(e.convert(e.bytes_new_from_slice(b"native")?)?),
        Metadata::AlphaNum4(asset) => {
            let mut res: Bytes = asset.asset_code.into();
            res.push(b':')?;
            let issuer_id = e.to_u256_from_account(&asset.issuer)?;
            res.append(e.convert(e.bytes_new_from_slice(&issuer_id.0)?)?)?;
            Ok(res)
        }
        Metadata::AlphaNum12(asset) => {
            let mut res: Bytes = asset.asset_code.into();
            res.push(b':')?;
            let issuer_id = e.to_u256_from_account(&asset.issuer)?;
            res.append(e.convert(e.bytes_new_from_slice(&issuer_id.0)?)?)?;
            Ok(res)
        }
    }
}

// Metering: *mostly* covered by components.`bytes_new_from_slice` and `Bytes` not covered.
pub fn read_symbol(e: &Host) -> Result<Bytes, HostError> {
    match read_metadata(e)? {
        Metadata::Token(token) => Ok(token.symbol),
        Metadata::Native => Ok(e.convert(e.bytes_new_from_slice(b"native")?)?),
        Metadata::AlphaNum4(asset) => Ok(asset.asset_code.into()),
        Metadata::AlphaNum12(asset) => Ok(asset.asset_code.into()),
    }
}

// Metering: covered by components
pub fn read_decimal(e: &Host) -> Result<u32, HostError> {
    match read_metadata(e)? {
        Metadata::Token(token) => Ok(token.decimals),
        Metadata::Native | Metadata::AlphaNum4(_) | Metadata::AlphaNum12(_) => Ok(7),
    }
}
