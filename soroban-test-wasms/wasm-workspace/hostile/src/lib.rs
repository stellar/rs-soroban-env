#![no_std]
use soroban_sdk::{contractimpl, Bytes, Env};

pub struct Contract;

// The Ackermann function
fn ack(m: u32, n: u32) -> u32 {
    if m == 0 {
        return n + 1;
    }
    if n == 0 {
        return ack(m - 1, 1);
    }
    return ack(m - 1, ack(m, n - 1));
}

fn stacksmash(n: u64) {
    if n > 0 {
        stacksmash(n + n / 2)
    }
}

static GLOBAL_BUF: [u32; 10] = [0; 10];

// We use a pub mutable global here so the optimizer
// doesn't get clever and try to prove the OOB access
// is certain and reduce it to a single `unreachable`.
pub static mut BAD_IDX: usize = 100;

#[contractimpl]
impl Contract {
    pub fn iloop() {
        loop {}
    }

    pub fn badack() {
        ack(4, 8);
    }

    pub fn ssmash() {
        stacksmash(2)
    }

    pub fn oob1() -> u32 {
        GLOBAL_BUF[unsafe { BAD_IDX }] + 1
    }

    pub fn oob2() -> u32 {
        let local_buf: [u32; 10] = [0; 10];
        local_buf[unsafe { BAD_IDX }] + 1
    }

    pub fn objs(env: Env) {
        let local_buf: [u8; 1024] = [0; 1024];
        for _ in 0..100000 {
            Bytes::from_slice(&env, &local_buf);
        }
    }
}
