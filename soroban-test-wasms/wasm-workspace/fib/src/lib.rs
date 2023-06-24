#![no_std]
use soroban_sdk::{contract, contractimpl};

#[contract]
pub struct Contract;

fn fib(n: u32) -> u32 {
    if n < 2 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

#[contractimpl]
impl Contract {
    pub fn main() {
        fib(30);
    }
}
