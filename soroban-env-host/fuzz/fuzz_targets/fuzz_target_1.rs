#![no_main]

use libfuzzer_sys::fuzz_target;
use arbitrary::Arbitrary;
use soroban_env_host::{Host, Symbol,  budget::AsBudget, Env};
use soroban_env_host_fuzz::wasm_module_with_linear_memory_logging;

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
    let host = Host::test_host_with_recording_footprint();
    host.enable_debug().unwrap();
    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());

    host.as_budget().reset_limits(2_000_000, 500_000).unwrap();
    let _res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("test").unwrap(),
        host.test_vec_obj(&[input.msg_pos, input.msg_len, input.vals_pos, input.vals_len]).unwrap(),
    ).unwrap();

    let cpu = host.as_budget().get_cpu_insns_consumed().unwrap();
    let mem = host.as_budget().get_mem_bytes_consumed().unwrap();

    assert_eq!((cpu,mem), (1073488, 203274) );
});
