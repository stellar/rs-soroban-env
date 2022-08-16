use crate::host::Host;
use crate::native_contract::base_types::Bytes;
use crate::native_contract::token::error::Error;
use crate::native_contract::token::public_types::Metadata;
use crate::native_contract::token::storage_types::DataKey;
use soroban_env_common::{CheckedEnv, EnvBase, TryIntoVal};

pub fn write_metadata(e: &Host, metadata: Metadata) -> Result<(), Error> {
    let key = DataKey::Metadata;
    e.put_contract_data(key.try_into_val(e)?, metadata.try_into_val(e)?)?;
    Ok(())
}

pub fn read_name(e: &Host) -> Result<Bytes, Error> {
    let key = DataKey::Metadata;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    let metadata: Metadata = rv.in_env(e).try_into()?;
    match metadata {
        Metadata::Token(token) => Ok(token.name),
        Metadata::Native => Ok(e.binary_new_from_slice(b"native").in_env(e).try_into()?),
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
    let key = DataKey::Metadata;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    let metadata: Metadata = rv.in_env(e).try_into()?;
    match metadata {
        Metadata::Token(token) => Ok(token.symbol),
        Metadata::Native => Ok(e.binary_new_from_slice(b"native").in_env(e).try_into()?),
        Metadata::AlphaNum4(asset) => Ok(asset.asset_code.into()),
        Metadata::AlphaNum12(asset) => Ok(asset.asset_code.into()),
    }
}

pub fn read_decimal(e: &Host) -> Result<u32, Error> {
    let key = DataKey::Metadata;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    let metadata: Metadata = rv.in_env(e).try_into()?;
    match metadata {
        Metadata::Token(token) => Ok(token.decimals),
        Metadata::Native | Metadata::AlphaNum4(_) | Metadata::AlphaNum12(_) => Ok(7),
    }
}
