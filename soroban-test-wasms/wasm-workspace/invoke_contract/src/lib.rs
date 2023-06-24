#![no_std]
use soroban_sdk::{contract, contractimpl, vec, Address, Env, IntoVal, Symbol};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn add(env: Env, a: i32, b: i32) -> i32 {
        env.events().publish((Symbol::short("add"),), (a, b));
        a + b
    }

    pub fn add_with(env: Env, x: i32, y: i32, contract_id: Address) -> i32 {
        env.events()
            .publish((Symbol::short("add_with"),), (x, y, contract_id.clone()));
        env.invoke_contract(
            &contract_id,
            &Symbol::short("add"),
            vec![&env, x.into_val(&env), y.into_val(&env)],
        )
    }
}
