#![no_std]
use soroban_sdk::{
    auth::Context, contract, contractimpl, contracttype, BytesN, Env, Val, Vec,
};

#[derive(Clone)]
#[contracttype]
pub enum DataKey {
    Allowed,
}

#[contract]
struct ConditionalAccount;

// Account that has `__check_auth` implementation that depends
// on mutable ledger state.
// For most of the accounts the result of `__check_auth` probably
// wouldn't change within the scope of a single transaction. However,
// we still need to properly support the cases when `__check_auth`
// might pass after some precondition is fulfilled (in combination
// with `try_call`).
#[contractimpl]
impl ConditionalAccount {
    // Makes next `__check_auth` pass.
    pub fn allow(env: Env) {
        env.storage().persistent().set(&DataKey::Allowed, &true);
    }

    #[allow(non_snake_case)]
    pub fn __check_auth(
        env: Env,
        _signature_payload: BytesN<32>,
        _signature: Val,
        _auth_context: Vec<Context>,
    ) {
        if let Some(true) = env.storage().persistent().get(&DataKey::Allowed) {
            env.storage().persistent().set(&DataKey::Allowed, &false);
        } else {
            panic!("not allowed");
        }
    }
}
