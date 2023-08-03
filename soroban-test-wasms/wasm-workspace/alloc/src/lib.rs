#![no_std]
use soroban_sdk::{contract, contractimpl, Env};

extern crate alloc;

#[contract]
pub struct AllocContract;

struct Large {
    x: u32,
    space: [u8;4096]
}

#[contractimpl]
impl AllocContract {
    /// Allocates a temporary vector holding values (0..count), then computes
    /// and returns their sum. Also allocates these values in a "large"
    /// structure (with a bunch of pointless padding) to ensure the contract
    /// allocates lots of memory.
    pub fn sum(_env: Env, count: u32) -> u32 {
        let mut v1 = alloc::vec![];
        for i in 0..count {
            v1.push(Large{x: i, space: [0u8; 4096]})
        }
        v1.iter().map(|l| l.x + l.space[0] as u32).sum()
    }
}
