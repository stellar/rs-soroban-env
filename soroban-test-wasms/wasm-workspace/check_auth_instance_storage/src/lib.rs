#![no_std]
use soroban_sdk::{
    auth::Context, contract, contractimpl, symbol_short, BytesN, Env, IntoVal, Symbol, Val, Vec,
};

#[contract]
struct AccountContract;

const KEY: Symbol = symbol_short!("key");

#[contractimpl]
impl AccountContract {
    pub fn pre_auth(env: Env, auth_calls: u32) {
        for i in 0..auth_calls {
            env.storage()
                .instance()
                .update(&KEY, |v: Option<u32>| v.unwrap_or_default() + 1);
            env.current_contract_address()
                .require_auth_for_args((i,).into_val(&env));
        }
    }

    pub fn post_auth(env: Env, auth_calls: u32) {
        for i in 0..auth_calls {
            env.current_contract_address()
                .require_auth_for_args((i,).into_val(&env));
            env.storage()
                .instance()
                .update(&KEY, |v: Option<u32>| v.unwrap_or_default() + 1);
        }
    }

    pub fn pre_post(env: Env, auth_calls: u32) {
        for i in 0..auth_calls {
            env.storage()
                .instance()
                .update(&KEY, |v: Option<u32>| v.unwrap_or_default() + 1);
            env.current_contract_address()
                .require_auth_for_args((i,).into_val(&env));
            env.storage()
                .instance()
                .update(&KEY, |v: Option<u32>| v.unwrap_or_default() + 1);
        }
    }

    pub fn get_key(env: Env) -> u32 {
        env.storage().instance().get(&KEY).unwrap()
    }

    #[allow(non_snake_case)]
    pub fn __check_auth(
        env: Env,
        _signature_payload: BytesN<32>,
        _signature: Val,
        _auth_context: Vec<Context>,
    ) {
        env.storage()
            .instance()
            .update(&KEY, |v: Option<u32>| v.unwrap_or_default() + 1);
    }
}
