#![no_std]
use soroban_sdk::{contract, contractimpl, contracttype, Env, Vec};

#[derive(Clone, Debug)]
#[contracttype]
pub enum DataKey {
    List,
    U32,
    U64,
}

#[contract]
pub struct Test;

#[contractimpl]
impl Test {
    pub fn set_list(env: Env, list: Vec<u32>) {
        env.storage().persistent().set(&DataKey::List, &list);
    }

    pub fn get_list(env: Env) -> Vec<u32> {
        env.storage()
            .persistent()
            .get(&DataKey::List)
            .unwrap_or(Vec::new(&env))
    }

    pub fn set_u32(env: Env, v: u32) {
        env.storage().persistent().set(&DataKey::U32, &v);
    }
    pub fn get_u32(env: Env) {
        env.storage().persistent().get(&DataKey::U32).unwrap()
    }

    pub fn set_u64(env: Env, v: u64) {
        env.storage().persistent().set(&DataKey::U64, &v);
    }
    pub fn get_u64(env: Env) {
        env.storage().persistent().get(&DataKey::U64).unwrap()
    }
}

// mod test;
