#![no_std]
use soroban_sdk::{
    auth::Context, contract, contractimpl, contracttype, BytesN, Env, IntoVal, Val, Vec,
};

#[contract]
struct AccountContract;

#[derive(Clone)]
#[contracttype]
pub enum DataKey {
    Key,
    FailAt,
}

#[contractimpl]
impl AccountContract {
    pub fn pre_auth(env: Env, auth_calls: u32) {
        for i in 0..auth_calls {
            bump_storage(&env);
            env.current_contract_address()
                .require_auth_for_args((i,).into_val(&env));
        }
    }

    pub fn post_auth(env: Env, auth_calls: u32) {
        for i in 0..auth_calls {
            env.current_contract_address()
                .require_auth_for_args((i,).into_val(&env));
            bump_storage(&env);
        }
    }

    pub fn pre_post(env: Env, auth_calls: u32) {
        for i in 0..auth_calls {
            bump_storage(&env);
            env.current_contract_address()
                .require_auth_for_args((i,).into_val(&env));
            bump_storage(&env);
        }
    }

    pub fn fail_at(env: Env, fail_at: u32) {
        env.storage().instance().set(&DataKey::FailAt, &fail_at);
    }

    pub fn get_key(env: Env) -> u32 {
        env.storage()
            .instance()
            .get(&DataKey::Key)
            .unwrap_or_default()
    }

    #[allow(non_snake_case)]
    pub fn __check_auth(
        env: Env,
        _signature_payload: BytesN<32>,
        _signature: Val,
        _auth_context: Vec<Context>,
    ) {
        bump_storage(&env);
    }
}

fn bump_storage(env: &Env) {
    let current: u32 = env
        .storage()
        .instance()
        .get(&DataKey::Key)
        .unwrap_or_default();
    if let Some(fail_at) = env.storage().instance().get(&DataKey::FailAt) {
        if current == fail_at {
            panic!("failing at requested point");
        }
    }
    env.storage().instance().set(&DataKey::Key, &(current + 1));
}
