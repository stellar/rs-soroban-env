#![no_std]

use soroban_sdk::{contract, contractimpl, Env};

#[contract]
pub struct TestConstructorWithReturnValue;

#[contractimpl]
impl TestConstructorWithReturnValue {
    pub fn __constructor(_env: Env) -> i32 {
        0
    }
}
