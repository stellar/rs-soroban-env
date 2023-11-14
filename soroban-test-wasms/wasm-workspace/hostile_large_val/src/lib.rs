#![no_std]

use soroban_env_common::Val;
use soroban_sdk::{contract, contractimpl, Env, log, symbol_short, Vec};
use soroban_sdk::xdr::ToXdr;

#[contract]
pub struct Contract;

// Build a value that has O(width ^ depth) size when serialized.
// This has just O(width * height) time/space complexity before
// serialization.
fn build_val(env: &Env, width: u32, depth: u32) -> Val {
    let mut res = Vec::<Val>::new(env);
    for _ in 0..width {
        res.push_back(123_u32.into());
    }
    for _ in 1..depth {
        let element_val: Val = res.into();
        let mut curr = Vec::<Val>::new(env);
        for _ in 0..width {
            curr.push_back(element_val);
        }
        res = curr;
    }
    res.into()
}

#[contractimpl]
impl Contract {
    pub fn serialize(env: Env, width: u32, depth: u32) {
        let val = build_val(&env, width, depth);
        val.to_xdr(&env);
    }

    pub fn iter(env: Env, width: u32, depth: u32) {
        let val = build_val(&env, width, depth);
        loop {
            val.to_xdr(&env);
        }
    }

    pub fn store(env: Env, width: u32, depth: u32) {
        env.storage()
            .persistent()
            .set(&symbol_short!("val"), &build_val(&env, width, depth));
    }

    // There is no way large non-serializable value can get to the footprint.
    // However, we can make sure that this gracefully fails while trying to
    // verify that the value is missing from the footprint.
    pub fn store_key(env: Env, width: u32, depth: u32) {
        env.storage()
            .persistent()
            .set(&build_val(&env, width, depth), &symbol_short!("val"));
    }

    pub fn store_instance(env: Env, width: u32, depth: u32) {
        env.storage()
            .instance()
            .set(&symbol_short!("val"), &build_val(&env, width, depth));
    }

    pub fn store_instance_key(env: Env, width: u32, depth: u32) {
        env.storage()
            .instance()
            .set(&build_val(&env, width, depth), &symbol_short!("val"));
    }

    pub fn event_topic(env: Env, width: u32, depth: u32) {
        env.events().publish((build_val(&env, width, depth), ), 123_u32);
    }

    pub fn event_data(env: Env, width: u32, depth: u32) {
        env.events().publish((123_u32, ), build_val(&env, width, depth));
    }

    pub fn return_value(env: Env, width: u32, depth: u32) -> Val {
        build_val(&env, width, depth)
    }

    pub fn diagnostics(env: Env, width: u32, depth: u32) {
        let v = build_val(&env, width, depth);
        log!(&env, "{}", v);
    }
}
