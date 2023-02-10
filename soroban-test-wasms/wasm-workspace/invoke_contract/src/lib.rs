#![no_std]
use soroban_sdk::{contractimpl, vec, BytesN, Env, IntoVal, Symbol};

pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn add(env: Env, a: i32, b: i32) -> i32 {
        env.events().publish((Symbol::from_str("add"),), (a, b));
        a + b
    }

    pub fn add_with(env: Env, x: i32, y: i32, contract_id: BytesN<32>) -> i32 {
        env.events()
            .publish((Symbol::from_str("add_with"),), (x, y, contract_id.clone()));
        env.invoke_contract(
            &contract_id,
            &Symbol::from_str("add"),
            vec![&env, x.into_val(&env), y.into_val(&env)],
        )
    }
}
