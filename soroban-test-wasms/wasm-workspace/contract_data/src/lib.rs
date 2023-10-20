#![no_std]
use soroban_sdk::{contract, contractimpl, Bytes, Env, Symbol};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn put_persistent(e: Env, key: Symbol, val: u64) {
        e.storage().persistent().set(&key, &val);
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

    pub fn extend_persistent(
        e: Env,
        key: Symbol,
        threshold: u32,
        extend_to: u32,
    ) {
        e.storage()
            .persistent()
            .extend_ttl(&key, threshold, extend_to)
    }

    pub fn put_temporary(e: Env, key: Symbol, val: u64) {
        e.storage().temporary().set(&key, &val)
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

    pub fn extend_temporary(
        e: Env,
        key: Symbol,
        threshold: u32,
        extend_to: u32,
    ) {
        e.storage()
            .temporary()
            .extend_ttl(&key, threshold, extend_to)
    }

    pub fn put_instance(e: Env, key: Symbol, val: u64) {
        e.storage().instance().set(&key, &val)
    }

    pub fn del_instance(e: Env, key: Symbol) {
        e.storage().instance().remove(&key)
    }

    pub fn has_instance(e: Env, key: Symbol) -> bool {
        e.storage().instance().has(&key)
    }

    pub fn get_instance(e: Env, key: Symbol) -> u64 {
        e.storage().instance().get(&key).unwrap()
    }

    pub fn extend_instance(e: Env, threshold: u32, extend_to: u32) {
        e.storage()
            .instance()
            .extend_ttl(threshold, extend_to)
    }

    pub fn replace_with_bytes_and_extend(
        e: Env,
        key: Symbol,
        num_kilo_bytes: u32,
        threshold: u32,
        extend_to: u32,
    ) {
        let slice = [0_u8; 1024];
        let mut bytes = Bytes::new(&e);
        for _ in 0..num_kilo_bytes {
            bytes.extend_from_slice(&slice);
        }
        e.storage().persistent().set(&key, &bytes);
        e.storage()
            .persistent()
            .extend_ttl(&key, threshold, extend_to)
    }
}
