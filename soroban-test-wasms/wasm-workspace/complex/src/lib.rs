#![no_std]
use soroban_sdk::{contractimpl, contracttype, Bytes, Env, Symbol, Vec};

// This is a "complex" contract that uses a nontrivial amount of the host
// interface from the guest: UDTs (thus maps), vectors, byte arrays and linear
// memory, hashing, logging, event-emitting, contract data. It's intended as a
// smoke test of host functionality.
//
// It has a write-footprint of a single LE called Symbol("data") and exposes a
// single 0-ary method called "go()".
//
// It should run to completion, write an LE, log a value, and emit an event.

pub struct Contract;

#[contracttype]
struct MyLedger {
    passphrase: Bytes,
    version: u32,
    seq: u32,
    time: u64,
}

#[contractimpl]
impl Contract {
    pub fn go(e: Env) {
        let ledger = e.ledger();
        let my_ledger = MyLedger {
            passphrase: ledger.network_passphrase(),
            version: ledger.protocol_version(),
            seq: ledger.sequence(),
            time: ledger.timestamp(),
        };
        let data = Symbol::from_str("data");
        let hash = e.crypto().sha256(&my_ledger.passphrase);
        let mut buf: [u8; 32] = [0; 32];
        hash.copy_into_slice(&mut buf);
        let vec_with_half_hash = Vec::from_slice(&e, &[Bytes::from_slice(&e, &buf[0..16])]);
        e.events().publish((data,), hash);
        e.log_value(vec_with_half_hash);
        e.storage().set(data, my_ledger);
    }
}
