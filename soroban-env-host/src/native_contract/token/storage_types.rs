use crate::native_contract::base_types::Map;
use crate::native_contract::token::public_types::Identifier;
use soroban_env_common::TryIntoVal;
use soroban_native_sdk_macros::contracttype;

#[contracttype]
pub struct AllowanceDataKey {
    pub from: Identifier,
    pub spender: Identifier,
}

#[contracttype]
pub enum DataKey {
    Allowance(AllowanceDataKey),
    Balance(Identifier),
    Nonce(Identifier),
    State(Identifier),
    Admin,
    Metadata,
}
