use soroban_native_sdk_macros::contracttype;

use crate::{host::Host, HostError};
use soroban_env_common::{Env, EnvBase, TryFromVal, TryIntoVal};

use crate::native_contract::base_types::{Bytes, Symbol};

use super::{asset_info::read_asset_info, public_types::AssetInfo};

const METADATA_KEY: Symbol = Symbol::short("METADATA");

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
    e.put_contract_data(METADATA_KEY.try_into_val(e)?, metadata.try_into_val(e)?)?;
    Ok(())
}

pub fn read_name(e: &Host) -> Result<Bytes, HostError> {
    let metadata: TokenMetadata = e
        .get_contract_data(METADATA_KEY.try_into_val(e)?)?
        .try_into_val(e)?;
    Ok(metadata.name)
}

pub fn read_symbol(e: &Host) -> Result<Bytes, HostError> {
    let metadata: TokenMetadata = e
        .get_contract_data(METADATA_KEY.try_into_val(e)?)?
        .try_into_val(e)?;
    Ok(metadata.symbol)
}
