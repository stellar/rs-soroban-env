#![no_std]
use soroban_sdk::{contract, contractimpl, Env, Symbol};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn add(env: Env, a: i32, b: i32) -> i32 {
        env.events().publish((Symbol::short("add"),), (a, b));
        let a = a as f32;
        let b = b as f32;
        (a + b) as i32
    }
}
