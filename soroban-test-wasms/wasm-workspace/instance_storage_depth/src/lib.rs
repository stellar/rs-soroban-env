#![no_std]
use soroban_sdk::{
    auth::Context, contract, contractimpl, symbol_short, BytesN, Env, IntoVal, Symbol, Val, Vec,
};

const VAL_KEY: Symbol = symbol_short!("val");

fn nested_val(env: &Env, depth: u32) -> Val {
    let mut v: Val = 0_u32.into_val(env);
    for _ in 1..depth {
        v = Vec::<Val>::from_array(env, [v]).into_val(env);
    }
    v
}

#[contract]
struct Contract;

#[contractimpl]
impl Contract {
    pub fn __constructor(env: Env, depth: u32) {
        if depth > 0 {
            env.storage()
                .instance()
                .set(&VAL_KEY, &nested_val(&env, depth));
        }
    }

    pub fn put_val(env: Env, depth: u32) {
        env.storage()
            .instance()
            .set(&VAL_KEY, &nested_val(&env, depth));
    }

    pub fn put_key(env: Env, depth: u32) {
        env.storage()
            .instance()
            .set(&nested_val(&env, depth), &1_u32);
    }

    pub fn put_small(env: Env) {
        env.storage().instance().set(&symbol_short!("small"), &1_u32);
    }

    // Writes a value of the given depth into instance storage, then requires
    // auth for the current contract, which persists the instance storage before
    // the self-reentrant `__check_auth` call. The value is removed before
    // returning, so that the instance storage persisted at the end of the
    // call doesn't contain it.
    pub fn put_val_and_self_auth(env: Env, depth: u32) {
        env.storage()
            .instance()
            .set(&VAL_KEY, &nested_val(&env, depth));
        env.current_contract_address().require_auth();
        env.storage().instance().remove(&VAL_KEY);
    }

    #[allow(non_snake_case)]
    pub fn __check_auth(
        env: Env,
        _signature_payload: BytesN<32>,
        _signature: Val,
        _auth_context: Vec<Context>,
    ) {
        // The value written before the self-reentry must be visible.
        if !env.storage().instance().has(&VAL_KEY) {
            panic!("value is missing in __check_auth");
        }
    }
}
