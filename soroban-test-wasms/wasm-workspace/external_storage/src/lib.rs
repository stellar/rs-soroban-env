#![no_std]
use soroban_sdk::{contract, contractimpl, Address, Env, Symbol};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn has_persistent(env: Env, contract: Address, key: Symbol) -> bool {
        env.storage().persistent().has_external(&contract, &key)
    }

    pub fn get_persistent(env: Env, contract: Address, key: Symbol) -> u64 {
        env.storage()
            .persistent()
            .get_external(&contract, &key)
            .unwrap()
    }

    pub fn has_temporary(env: Env, contract: Address, key: Symbol) -> bool {
        env.storage().temporary().has_external(&contract, &key)
    }

    pub fn get_temporary(env: Env, contract: Address, key: Symbol) -> u64 {
        env.storage()
            .temporary()
            .get_external(&contract, &key)
            .unwrap()
    }

    pub fn has_instance(env: Env, contract: Address, key: Symbol) -> bool {
        env.storage().instance().has_external(&contract, &key)
    }

    pub fn get_instance(env: Env, contract: Address, key: Symbol) -> u64 {
        env.storage()
            .instance()
            .get_external(&contract, &key)
            .unwrap()
    }
}
