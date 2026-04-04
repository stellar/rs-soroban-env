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

    pub fn extend_persistent_v2(
        e: Env,
        key: Symbol,
        extend_to: u32,
        min_extension: u32,
        max_extension: u32,
    ) {
        e.storage()
            .persistent()
            .extend_ttl_with_limits(&key, extend_to, min_extension, max_extension)
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

    pub fn extend_temporary_v2(
        e: Env,
        key: Symbol,
        extend_to: u32,
        min_extension: u32,
        max_extension: u32,
    ) {
        e.storage()
            .temporary()
            .extend_ttl_with_limits(&key, extend_to, min_extension, max_extension)
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

    pub fn extend_instance_v2(e: Env, extend_to: u32, min_extension: u32, max_extension: u32) {
        e.storage()
            .instance()
            .extend_ttl_with_limits(extend_to, min_extension, max_extension)
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

    /// Performs a series of storage operations on the same key within a single
    /// contract call. This exercises the copy-on-write semantics and ensures
    /// that multiple modifications at the same frame depth don't create duplicate
    /// frames in the entry and TTL stacks.
    ///
    /// storage_type: 0 = Temporary, 1 = Persistent, 2 = Instance
    pub fn stress_storage(e: Env, storage_type: u32, key: Symbol) {
        match storage_type {
            0 => {
                // Temporary storage
                let storage = e.storage().temporary();
                // Write initial value
                storage.set(&key, &1_u64);
                // Read it back
                let _v: u64 = storage.get(&key).unwrap();
                // Overwrite with new value
                storage.set(&key, &2_u64);
                // Extend TTL
                storage.extend_ttl(&key, 100, 1000);
                // Overwrite again
                storage.set(&key, &3_u64);
                // Extend TTL again (same frame depth)
                storage.extend_ttl(&key, 100, 2000);
                // Delete
                storage.remove(&key);
                // Write again (re-create after deletion)
                storage.set(&key, &4_u64);
                // Extend TTL after re-creation
                storage.extend_ttl(&key, 100, 3000);
            }
            1 => {
                // Persistent storage
                let storage = e.storage().persistent();
                // Write initial value
                storage.set(&key, &1_u64);
                // Read it back
                let _v: u64 = storage.get(&key).unwrap();
                // Overwrite with new value
                storage.set(&key, &2_u64);
                // Extend TTL
                storage.extend_ttl(&key, 100, 1000);
                // Overwrite again
                storage.set(&key, &3_u64);
                // Extend TTL again (same frame depth)
                storage.extend_ttl(&key, 100, 2000);
                // Delete
                storage.remove(&key);
                // Write again (re-create after deletion)
                storage.set(&key, &4_u64);
                // Extend TTL after re-creation
                storage.extend_ttl(&key, 100, 3000);
            }
            2 => {
                // Instance storage
                let storage = e.storage().instance();
                // Write initial value
                storage.set(&key, &1_u64);
                // Read it back
                let _v: u64 = storage.get(&key).unwrap();
                // Overwrite with new value
                storage.set(&key, &2_u64);
                // Extend instance TTL
                e.storage().instance().extend_ttl(100, 1000);
                // Overwrite again
                storage.set(&key, &3_u64);
                // Extend instance TTL again (same frame depth)
                e.storage().instance().extend_ttl(100, 2000);
                // Delete
                storage.remove(&key);
                // Write again (re-create after deletion)
                storage.set(&key, &4_u64);
                // Extend instance TTL after re-creation
                e.storage().instance().extend_ttl(100, 3000);
            }
            _ => panic!("invalid storage_type"),
        }
    }
}
