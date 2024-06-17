#![no_std]
use soroban_sdk::{contract, contractimpl, contracttype, Env, Vec};

#[derive(Clone, Debug)]
#[contracttype]
pub enum DataKey {
    List,
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
}

// mod test;
