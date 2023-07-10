use crate::native_contract::base_types::Address;
use soroban_env_common::TryIntoVal;
use soroban_native_sdk_macros::contracttype;

pub(crate) const INSTANCE_BUMP_AMOUNT: u32 = 34560; // 2 hours
pub(crate) const BALANCE_BUMP_AMOUNT: u32 = 518400; // 30 days

#[contracttype]
pub struct AllowanceDataKey {
    pub from: Address,
    pub spender: Address,
}

#[contracttype]
pub struct AllowanceValue {
    pub amount: i128,
    pub expiration_ledger: u32,
}

#[contracttype]
pub struct BalanceValue {
    pub amount: i128,
    pub authorized: bool,
    pub clawback: bool,
}

/// Keys for the persistent data associated with token users.
#[contracttype]
pub enum DataKey {
    Allowance(AllowanceDataKey),
    Balance(Address),
}

/// Keys for token instance data.
#[contracttype]
pub enum InstanceDataKey {
    Admin,
    AssetInfo,
}
