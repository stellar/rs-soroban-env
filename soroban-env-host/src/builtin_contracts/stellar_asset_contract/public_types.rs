use crate::{
    builtin_contracts::base_types::{BytesN, String},
    xdr::Asset,
    Host, HostError, TryFromVal, TryIntoVal,
};
use soroban_builtin_sdk_macros::contracttype;

#[derive(Clone)]
#[contracttype]
pub(crate) struct AlphaNum4AssetInfo {
    pub asset_code: String,
    pub issuer: BytesN<32>,
}

#[derive(Clone)]
#[contracttype]
pub(crate) struct AlphaNum12AssetInfo {
    pub asset_code: String,
    pub issuer: BytesN<32>,
}

#[derive(Clone)]
#[contracttype]
pub(crate) enum AssetInfo {
    Native,
    AlphaNum4(AlphaNum4AssetInfo),
    AlphaNum12(AlphaNum12AssetInfo),
}

impl TryFromVal<Host, AssetInfo> for Asset {
    type Error = HostError;

    fn try_from_val(env: &Host, v: &AssetInfo) -> Result<Self, Self::Error> {
        match v {
            AssetInfo::Native => Ok(Asset::Native),
            AssetInfo::AlphaNum4(asset) => {
                let issuer_account_id = env.account_id_from_bytesobj(asset.issuer.as_object())?;
                Ok(env.create_asset_4(asset.asset_code.to_array()?, issuer_account_id))
            }
            AssetInfo::AlphaNum12(asset) => {
                let issuer_account_id = env.account_id_from_bytesobj(asset.issuer.as_object())?;
                Ok(env.create_asset_12(asset.asset_code.to_array()?, issuer_account_id))
            }
        }
    }
}
