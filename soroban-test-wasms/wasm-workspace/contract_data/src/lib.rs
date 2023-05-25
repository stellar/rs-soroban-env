#![no_std]
use soroban_sdk::{contractimpl, Env, Symbol};

pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn put_unique(e: Env, key: Symbol, val: u64, flags: Option<u32>) {
        e.storage().unique().set(&key, &val, flags)
    }

    pub fn del_unique(e: Env, key: Symbol) {
        e.storage().unique().remove(&key)
    }

    pub fn has_unique(e: Env, key: Symbol) -> bool {
        e.storage().unique().has(&key)
    }

    pub fn get_unique(e: Env, key: Symbol) -> u64 {
        e.storage().unique().get(&key).unwrap().unwrap()
    }

    pub fn bump_unique(e: Env, key: Symbol, min_to_live: u32) {
        e.storage().unique().bump(&key, min_to_live)
    }

    pub fn put_recreatable(e: Env, key: Symbol, val: u64, flags: Option<u32>) {
        e.storage().recreatable().set(&key, &val, flags)
    }

    pub fn del_recreatable(e: Env, key: Symbol) {
        e.storage().recreatable().remove(&key)
    }

    pub fn has_recreatable(e: Env, key: Symbol) -> bool {
        e.storage().recreatable().has(&key)
    }

    pub fn get_recreatable(e: Env, key: Symbol) -> u64 {
        e.storage().recreatable().get(&key).unwrap().unwrap()
    }

    pub fn bump_recreatable(e: Env, key: Symbol, min_to_live: u32) {
        e.storage().recreatable().bump(&key, min_to_live)
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
        e.storage().temporary().get(&key).unwrap().unwrap()
    }

    pub fn bump_temporary(e: Env, key: Symbol, min_to_live: u32) {
        e.storage().temporary().bump(&key, min_to_live)
    }
}
