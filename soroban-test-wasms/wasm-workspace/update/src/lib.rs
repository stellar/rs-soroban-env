#![no_std]
use soroban_sdk::{contract, contractimpl, BytesN, Env};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn update(env: Env, wasm_hash: BytesN<32>) -> i32 {
        env.deployer().update_current_contract_wasm(wasm_hash);
        123
    }
}
