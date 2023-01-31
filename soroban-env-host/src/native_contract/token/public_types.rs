pub(crate) use crate::native_contract::base_types::{BytesN, Map};
use soroban_env_common::TryIntoVal;
use soroban_native_sdk_macros::contracttype;

#[derive(Clone)]
#[contracttype]
pub struct AlphaNum4Metadata {
    pub asset_code: BytesN<4>,
    pub issuer: BytesN<32>,
}

#[derive(Clone)]
#[contracttype]
pub struct AlphaNum12Metadata {
    pub asset_code: BytesN<12>,
    pub issuer: BytesN<32>,
}

#[derive(Clone)]
#[contracttype]
pub enum Metadata {
    Native,
    AlphaNum4(AlphaNum4Metadata),
    AlphaNum12(AlphaNum12Metadata),
}
