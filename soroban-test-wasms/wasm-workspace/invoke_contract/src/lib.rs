#![no_std]
use soroban_sdk::{contractimpl, vec, BytesN, Env, IntoVal, Symbol};

pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn add_with(env: Env, x: i32, y: i32, contract_id: BytesN<32>) -> i32 {
        env.invoke_contract(
            &contract_id,
            &Symbol::from_str("add"),
            vec![&env, x.into_env_val(&env), y.into_env_val(&env)],
        )
    }
}
