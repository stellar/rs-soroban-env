#![no_std]
use soroban_sdk::{contract, contractimpl, Bytes, BytesN, Env};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn upload(e: Env, wasm: Bytes) -> BytesN<32> {
        e.deployer().upload_contract_wasm(wasm)
    }
}
