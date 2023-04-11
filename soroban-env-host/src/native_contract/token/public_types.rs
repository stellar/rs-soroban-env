pub(crate) use crate::native_contract::base_types::BytesN;
use soroban_env_common::TryIntoVal;
use soroban_native_sdk_macros::contracttype;

#[derive(Clone)]
#[contracttype]
pub struct AlphaNum4AssetInfo {
    pub asset_code: BytesN<4>,
    pub issuer: BytesN<32>,
}

#[derive(Clone)]
#[contracttype]
pub struct AlphaNum12AssetInfo {
    pub asset_code: BytesN<12>,
    pub issuer: BytesN<32>,
}

#[derive(Clone)]
#[contracttype]
pub enum AssetInfo {
    Native,
    AlphaNum4(AlphaNum4AssetInfo),
    AlphaNum12(AlphaNum12AssetInfo),
}
