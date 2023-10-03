#![no_std]
use soroban_sdk::{contract, contractimpl, symbol_short, BytesN, Env};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn update(env: Env, wasm_hash: BytesN<32>) -> i32 {
        // Modify instance storage to make sure that both instance storage and
        // executable are updated.
        env.storage().instance().set(&symbol_short!("foo"), &111);
        env.deployer().update_current_contract_wasm(wasm_hash);        
        123
    }
}
