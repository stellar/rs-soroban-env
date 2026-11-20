#![no_main]

use libfuzzer_sys::fuzz_target;
use soroban_fuzz_targets::{zk_bn254::run_fuzz_target, FuzzResult};

fuzz_target!(|data: &[u8]| {
    if run_fuzz_target(data) == FuzzResult::InternalError {
        panic!("got internal error");
    }
});
