#![no_std]

use soroban_sdk::{contract, contractimpl, symbol_short, Env, Symbol};

#[contract]
pub struct TestEmptyConstructorContract;

const KEY: Symbol = symbol_short!("key");

#[contractimpl]
impl TestEmptyConstructorContract {
    pub fn __constructor(env: Env) {
        env.storage().persistent().set(&KEY, &1_u32);
        env.storage().instance().set(&KEY, &2_u32);
        env.storage().temporary().set(&KEY, &3_u32);
    }

    pub fn get_data(env: Env, key: Symbol) -> u32 {
        env.storage().persistent().get::<_, u32>(&key).unwrap()
            + env.storage().instance().get::<_, u32>(&key).unwrap()
            + env.storage().temporary().get::<_, u32>(&key).unwrap()
    }
}
