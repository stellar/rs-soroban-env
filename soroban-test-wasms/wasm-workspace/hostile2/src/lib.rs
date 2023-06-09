// These tests have access to the full Soroban environment,
// where the tests in `hostile` only have a basic wasmi VM.

#![no_std]
use soroban_sdk::{contractimpl, RawVal};

pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn largelog() {
        #[link(wasm_import_module = "x")]
        extern "C" {
            #[link_name = "_"]
            fn log_from_linear_memory(_: RawVal, _: RawVal, _: RawVal, _: RawVal) -> RawVal;
        }

        unsafe {
            let v = RawVal::from_u32(u32::MAX).into();
            log_from_linear_memory(v, v, v, v);
        }
    }
}
