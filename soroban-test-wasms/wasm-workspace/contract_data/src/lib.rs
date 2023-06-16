#![no_std]
use soroban_sdk::{contractimpl, Env, Symbol};

pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn put_persistent(e: Env, key: Symbol, val: u64, flags: Option<u32>) {
        e.storage().persistent().set(&key, &val, flags)
    }

    pub fn del_persistent(e: Env, key: Symbol) {
        e.storage().persistent().remove(&key)
    }

    pub fn has_persistent(e: Env, key: Symbol) -> bool {
        e.storage().persistent().has(&key)
    }

    pub fn get_persistent(e: Env, key: Symbol) -> u64 {
        e.storage().persistent().get(&key).unwrap()
    }

    pub fn bump_persistent(e: Env, key: Symbol, min_to_live: u32) {
        e.storage().persistent().bump(&key, min_to_live)
    }

    pub fn put_temporary(e: Env, key: Symbol, val: u64, flags: Option<u32>) {
        e.storage().temporary().set(&key, &val, flags)
    }

    pub fn del_temporary(e: Env, key: Symbol) {
        e.storage().temporary().remove(&key)
    }

    pub fn has_temporary(e: Env, key: Symbol) -> bool {
        e.storage().temporary().has(&key)
    }

    pub fn get_temporary(e: Env, key: Symbol) -> u64 {
        e.storage().temporary().get(&key).unwrap()
    }

    pub fn bump_temporary(e: Env, key: Symbol, min_to_live: u32) {
        e.storage().temporary().bump(&key, min_to_live)
    }
}
