#![no_std]
use soroban_sdk::{contract, contractimpl, Env};

extern crate alloc;

#[contract]
pub struct AllocContract;

#[contractimpl]
impl AllocContract {
    /// Allocates a temporary vector holding values (0..count), then computes and returns their sum.
    pub fn sum(_env: Env) {
        // let mut v1 = alloc::vec![];
        // v1.push(738);
        let _v: alloc::vec::Vec<u32> = alloc::vec::Vec::with_capacity(347);
    }
}
