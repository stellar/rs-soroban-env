use crate::host::Host;
use crate::native_contract::base_types::Bytes;
use crate::native_contract::token::error::Error;
use crate::native_contract::token::public_types::Metadata;
use crate::native_contract::token::storage_types::DataKey;
use soroban_env_common::{CheckedEnv, EnvBase, TryFromVal, TryIntoVal};

pub fn write_metadata(e: &Host, metadata: Metadata) -> Result<(), Error> {
    let key = DataKey::Metadata;
    e.put_contract_data(key.try_into_val(e)?, metadata.try_into_val(e)?)?;
    Ok(())
}

pub fn read_metadata(e: &Host) -> Result<Metadata, Error> {
    let key = DataKey::Metadata;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    Ok(rv.try_into_val(e)?)
}

pub fn has_metadata(e: &Host) -> Result<bool, Error> {
    let key = DataKey::Metadata;
    let rv = e.has_contract_data(key.try_into_val(e)?)?;
    Ok(rv.try_into()?)
}

pub fn read_name(e: &Host) -> Result<Bytes, Error> {
    match read_metadata(e)? {
        Metadata::Token(token) => Ok(token.name),
        Metadata::Native => Ok(Bytes::try_from_val(e, e.bytes_new_from_slice(b"native")?)?),
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

pub fn read_symbol(e: &Host) -> Result<Bytes, Error> {
    match read_metadata(e)? {
        Metadata::Token(token) => Ok(token.symbol),
        Metadata::Native => Ok(Bytes::try_from_val(e, e.bytes_new_from_slice(b"native")?)?),
        Metadata::AlphaNum4(asset) => Ok(asset.asset_code.into()),
        Metadata::AlphaNum12(asset) => Ok(asset.asset_code.into()),
    }
}

pub fn read_decimal(e: &Host) -> Result<u32, Error> {
    match read_metadata(e)? {
        Metadata::Token(token) => Ok(token.decimals),
        Metadata::Native | Metadata::AlphaNum4(_) | Metadata::AlphaNum12(_) => Ok(7),
    }
}
