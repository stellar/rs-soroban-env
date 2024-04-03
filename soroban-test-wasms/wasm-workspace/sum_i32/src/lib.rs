#![no_std]
use soroban_sdk::{contract, contractimpl, symbol_short, vec, Address, Env, IntoVal, Vec};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn sum(env: Env, add: Address, ints: Vec<i32>) -> i32 {
        let mut sum: i32 = 0;
        for i in ints {
            sum = env
                .invoke_contract::<i32>(
                    &add,
                    &symbol_short!("add"),
                    vec![&env, sum.into_val(&env), i.into_val(&env)],
                )
                .into();
        }
        sum
    }
}
