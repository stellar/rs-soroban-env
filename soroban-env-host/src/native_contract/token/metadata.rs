use soroban_native_sdk_macros::contracttype;
use stellar_strkey::ed25519;

use crate::{host::Host, HostError};
use soroban_env_common::{
    ConversionError, Env, EnvBase, StorageType, SymbolSmall, TryFromVal, TryIntoVal,
};

use crate::native_contract::base_types::String;

use super::{asset_info::read_asset_info, public_types::AssetInfo};

const METADATA_KEY: &str = "METADATA";

#[derive(Clone)]
#[contracttype]
pub struct TokenMetadata {
    pub decimal: u32,
    pub name: String,
    pub symbol: String,
}

pub const DECIMAL: u32 = 7;

pub fn set_metadata(e: &Host) -> Result<(), HostError> {
    let name_and_symbol: (String, String) = match read_asset_info(e)? {
        AssetInfo::Native => {
            let n = String::try_from_val(e, &e.string_new_from_slice("native")?)?;
            (n.clone(), n)
        }
        AssetInfo::AlphaNum4(asset) => {
            let symbol: String = asset.asset_code;
            let mut name = symbol.copy_to_rust_string(e)?;
            name.push(':');
            let k = ed25519::PublicKey(asset.issuer.to_array()?);
            name.push_str(k.to_string().as_str());
            (
                String::try_from_val(e, &e.string_new_from_slice(name.as_str())?)?,
                symbol,
            )
        }
        AssetInfo::AlphaNum12(asset) => {
            let symbol: String = asset.asset_code;
            let mut name = symbol.copy_to_rust_string(e)?;
            name.push(':');
            let k = ed25519::PublicKey(asset.issuer.to_array()?);
            name.push_str(k.to_string().as_str());
            (
                String::try_from_val(e, &e.string_new_from_slice(name.as_str())?)?,
                symbol,
            )
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
        StorageType::MERGEABLE,
        ().into(),
    )?;
    Ok(())
}

pub fn read_name(e: &Host) -> Result<String, HostError> {
    let key = SymbolSmall::try_from_str(METADATA_KEY)?;
    let metadata: TokenMetadata = e
        .get_contract_data(key.try_into_val(e)?, StorageType::MERGEABLE)?
        .try_into_val(e)?;
    Ok(metadata.name)
}

pub fn read_symbol(e: &Host) -> Result<String, HostError> {
    let key = SymbolSmall::try_from_str(METADATA_KEY)?;
    let metadata: TokenMetadata = e
        .get_contract_data(key.try_into_val(e)?, StorageType::MERGEABLE)?
        .try_into_val(e)?;
    Ok(metadata.symbol)
}
