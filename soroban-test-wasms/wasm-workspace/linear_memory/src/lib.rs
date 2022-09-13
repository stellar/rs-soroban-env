#![no_std]
use soroban_sdk::{contractimpl, Bytes, Env};

pub struct Contract;

#[contractimpl]
impl Contract {
    // Produce a bytes with 1 byte from each 8 bits of a 32-bit word.
    pub fn bin_word(e: Env, word: u32) -> Bytes {
        let buf: [u8; 4] = [
            (word >> 24) as u8,
            (word >> 16) as u8,
            (word >> 8) as u8,
            word as u8,
        ];
        Bytes::from_slice(&e, &buf)
    }

    // Transform a 4-byte bytes into a new one with each byte incremented,
    // via a temporary guest memory buffer.
    pub fn bin_inc(e: Env, bytes: Bytes) -> Bytes {
        let mut buf: [u8; 4] = [0; 4];
        bytes.copy_into_slice(&mut buf);
        for i in buf.iter_mut() {
            *i += 1;
        }
        Bytes::from_slice(&e, &buf)
    }
}
