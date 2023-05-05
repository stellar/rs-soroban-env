#![no_std]
use soroban_sdk::{contractimpl, Env, Symbol};

pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn put(e: Env, key: Symbol, val: u64) {
        e.storage().set(&key, &val)
    }

    pub fn del(e: Env, key: Symbol) {
        e.storage().remove(&key)
    }

    pub fn has(e: Env, key: Symbol) -> bool {
        e.storage().has(&key)
    }

    pub fn get(e: Env, key: Symbol) -> u64 {
        e.storage().get(&key).unwrap().unwrap()
    }
}
