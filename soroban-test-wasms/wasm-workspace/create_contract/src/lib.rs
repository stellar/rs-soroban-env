#![no_std]
use soroban_sdk::{contractimpl, Bytes, BytesN, Env};

pub struct Contract;

#[contractimpl]
impl Contract {
    // Note that anyone can create a contract here with any salt, so a users call to
    // this could be frontrun and the same salt taken.
    pub fn create(e: Env, wasm: Bytes, salt: Bytes) {
        let deployer = e.deployer().from_current_contract(&salt);
        let _contract_id = deployer.deploy(&wasm);
    }
}
