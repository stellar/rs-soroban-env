use crate::builtin_contracts::base_types::{BytesN, String};
use soroban_builtin_sdk_macros::contracttype;
use soroban_env_common::TryIntoVal;

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
