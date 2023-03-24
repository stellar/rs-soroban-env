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

    pub fn put_tmp(e: Env, key: i128, val: Symbol) {
        e.temp_storage().set(&key, &val)
    }

    pub fn del_tmp(e: Env, key: i128) {
        e.temp_storage().remove(&key)
    }

    pub fn has_tmp(e: Env, key: i128) -> bool {
        e.temp_storage().has(&key)
    }

    pub fn get_tmp(e: Env, key: i128) -> Symbol {
        e.temp_storage().get(&key).unwrap().unwrap()
    }
}
