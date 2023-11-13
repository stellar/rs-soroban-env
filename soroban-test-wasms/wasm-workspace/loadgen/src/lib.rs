#![no_std]
use soroban_sdk::{contract, contractimpl, Bytes, Env, U256};

#[contract]
pub struct Contract;

const EXTEND_LEDGER: u32 = 5_000_000;

#[contractimpl]
impl Contract {
    // This will write num_entries of size size_kilo_bytes. The initial entry will have a key of
    // starting_index, the next key starting_index + 1...
    pub fn write(e: Env, starting_index: u32, num_entries: u32, size_kilo_bytes: u32) {
        if num_entries == 0 || size_kilo_bytes == 0 {
            panic!("num_entries and size_kilo_bytes must be greater than 0");
        }

        let slice = [0_u8; 1024];
        let mut bytes = Bytes::new(&e);
        for _ in 0..size_kilo_bytes {
            bytes.extend_from_slice(&slice);
        }

        for i in starting_index..starting_index + num_entries {
            e.storage().persistent().set(&i, &bytes);
            e.storage()
                .persistent()
                .extend_ttl(&i, EXTEND_LEDGER, EXTEND_LEDGER);
        }
    }

    pub fn do_work(e: Env, guest_cycles: u64, host_cycles: u64) -> U256 {
        let mut val: u32 = 0;
        for _ in 0..guest_cycles {
            if u32::MAX == val {
                val = 0;
            }

            val += 1
        }

        let mut u256_val = U256::from_u32(&e, val);
        let u256_1 = U256::from_u32(&e, 1);
        for _ in 0..host_cycles {
            u256_val = u256_val.add(&u256_1);
        }

        // Return has data dependency on both values to make sure nothing gets optimized out
        u256_val
    }
}
