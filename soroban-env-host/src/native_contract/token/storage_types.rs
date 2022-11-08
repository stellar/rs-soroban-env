use crate::native_contract::base_types::Map;
use soroban_env_common::{TryIntoVal, xdr::ScAddress};
use soroban_native_sdk_macros::contracttype;

#[contracttype]
pub struct AllowanceDataKey {
    pub from: ScAddress,
    pub spender: ScAddress,
}

#[contracttype]
pub enum DataKey {
    Allowance(AllowanceDataKey),
    Balance(ScAddress),
    State(ScAddress),
    Admin,
    Metadata,
}
