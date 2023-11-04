#![no_std]
use soroban_sdk::{contract, contractimpl, vec, Address, Env, IntoVal, Symbol, symbol_short, Error};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn add(env: Env, a: i32, b: i32) -> i32 {
        env.events().publish((symbol_short!("add"),), (a, b));
        a + b
    }

    pub fn add_with(env: Env, x: i32, y: i32, contract_id: Address) -> i32 {
        env.events()
            .publish((symbol_short!("add_with"),), (x, y, contract_id.clone()));
        env.invoke_contract(
            &contract_id,
            &symbol_short!("add"),
            vec![&env, x.into_val(&env), y.into_val(&env)],
        )
    }
    
    // This is used through a single test case to test various rollback scenarios 
    pub fn invoke(env: Env, contract_id: Address, function_name: Symbol, token: Address) {
        assert!(env.try_invoke_contract::<(), Error>(
            &contract_id,
            &function_name,
            vec![&env, token.into_val(&env)],
        ).is_err());
    }
}
