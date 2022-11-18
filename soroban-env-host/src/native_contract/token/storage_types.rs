use crate::native_contract::base_types::{BigInt, Map};
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
    Admin,
    Metadata,
}

#[contracttype]
pub struct BalanceValue {
    pub amount: BigInt,
    pub authorized: bool,
}
