use soroban_native_sdk_macros::contracttype;

use crate::{host::Host, HostError};
use soroban_env_common::{Env, EnvBase, StorageType, SymbolSmall, TryFromVal, TryIntoVal};

use crate::native_contract::base_types::Bytes;

use super::{asset_info::read_asset_info, public_types::AssetInfo};

const METADATA_KEY: &str = "METADATA";

#[derive(Clone)]
#[contracttype]
pub struct TokenMetadata {
    pub decimal: u32,
    pub name: Bytes,
    pub symbol: Bytes,
}

pub const DECIMAL: u32 = 7;

pub fn set_metadata(e: &Host) -> Result<(), HostError> {
    let name_and_symbol: (Bytes, Bytes) = match read_asset_info(e)? {
        AssetInfo::Native => {
            let n = Bytes::try_from_val(e, &e.bytes_new_from_slice(b"native")?)?;
            (n.clone(), n)
        }
        AssetInfo::AlphaNum4(asset) => {
            let symbol: Bytes = asset.asset_code.into();
            let mut name = symbol.clone();
            name.push(b':')?;
            name.append(asset.issuer.into())?;
            (name, symbol)
        }
        AssetInfo::AlphaNum12(asset) => {
            let symbol: Bytes = asset.asset_code.into();
            let mut name = symbol.clone();
            name.push(b':')?;
            name.append(asset.issuer.into())?;
            (name, symbol)
        }
    };

    let metadata = TokenMetadata {
        decimal: DECIMAL,
        name: name_and_symbol.0,
        symbol: name_and_symbol.1,
    };

    let key = SymbolSmall::try_from_str(METADATA_KEY)?;
    e.put_contract_data(
        key.try_into_val(e)?,
        metadata.try_into_val(e)?,
        StorageType::RECREATABLE,
        ().into(),
    )?;
    Ok(())
}

pub fn read_name(e: &Host) -> Result<Bytes, HostError> {
    let key = SymbolSmall::try_from_str(METADATA_KEY)?;
    let metadata: TokenMetadata = e
        .get_contract_data(key.try_into_val(e)?, StorageType::RECREATABLE)?
        .try_into_val(e)?;
    Ok(metadata.name)
}

pub fn read_symbol(e: &Host) -> Result<Bytes, HostError> {
    let key = SymbolSmall::try_from_str(METADATA_KEY)?;
    let metadata: TokenMetadata = e
        .get_contract_data(key.try_into_val(e)?, StorageType::RECREATABLE)?
        .try_into_val(e)?;
    Ok(metadata.symbol)
}
