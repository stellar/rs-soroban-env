#![no_main]

use libfuzzer_sys::fuzz_target;
use fuzz_debug_logging::wasm_module_with_linear_memory_logging;
use soroban_sdk::arbitrary::arbitrary::{self, Arbitrary};
use soroban_sdk::arbitrary::fuzz_catch_panic;
use soroban_env_host::{Host, Symbol, SymbolSmall, budget::AsBudget, Env};

#[derive(Arbitrary, Debug)]
struct Input {
    msg_pos: u32,
    msg_len: u32,
    vals_pos: u32,
    vals_len: u32,    
}


fuzz_target!(|input: Input| {
    // fuzzed code goes here

    let wasm = wasm_module_with_linear_memory_logging();
    let host = Host::default();
    host.enable_debug().unwrap();
    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());

    host.as_budget().reset_limits(2_000_000, 500_000).unwrap();
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("test").unwrap(),
        host.test_vec_obj(&[input.msg_pos, input.msg_len, input.vals_pos, input.vals_len]).unwrap(),
    ).unwrap();
    // assert_eq!(SymbolSmall::try_from(res).unwrap().to_string(), "pass");
    // three debug events: fn_call, log, fn_return
    // assert!(
    //     !host.as_budget().shadow_mem_limit_exceeded()?
    //         && !host.as_budget().shadow_cpu_limit_exceeded()?
    // );
    // let actual = format!("{}", host.as_budget());
    // expected_budget.assert_eq(&actual);
});
