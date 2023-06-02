#![no_std]
use soroban_sdk::{contractimpl, Env, Symbol};

pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn put_exclusive(e: Env, key: Symbol, val: u64, flags: Option<u32>) {
        e.storage().exclusive().set(&key, &val, flags)
    }

    pub fn del_exclusive(e: Env, key: Symbol) {
        e.storage().exclusive().remove(&key)
    }

    pub fn has_exclusive(e: Env, key: Symbol) -> bool {
        e.storage().exclusive().has(&key)
    }

    pub fn get_exclusive(e: Env, key: Symbol) -> u64 {
        e.storage().exclusive().get(&key).unwrap().unwrap()
    }

    pub fn bump_exclusive(e: Env, key: Symbol, min_to_live: u32) {
        e.storage().exclusive().bump(&key, min_to_live)
    }

    pub fn put_mergeable(e: Env, key: Symbol, val: u64, flags: Option<u32>) {
        e.storage().mergeable().set(&key, &val, flags)
    }

    pub fn del_mergeable(e: Env, key: Symbol) {
        e.storage().mergeable().remove(&key)
    }

    pub fn has_mergeable(e: Env, key: Symbol) -> bool {
        e.storage().mergeable().has(&key)
    }

    pub fn get_mergeable(e: Env, key: Symbol) -> u64 {
        e.storage().mergeable().get(&key).unwrap().unwrap()
    }

    pub fn bump_mergeable(e: Env, key: Symbol, min_to_live: u32) {
        e.storage().mergeable().bump(&key, min_to_live)
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
