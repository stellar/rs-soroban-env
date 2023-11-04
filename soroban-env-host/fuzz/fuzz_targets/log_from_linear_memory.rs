#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use soroban_env_host::{
    budget::{AsBudget, Budget},
    xdr::{Hash, HostFunction, InvokeContractArgs, ScAddress, ScSymbol},
};
use soroban_env_host_fuzz::{
    storage_with_test_contract, test_host_with_storage_and_budget, test_scvec,
    wasm_module_with_linear_memory_logging, TEST_CONTRACT_ID, TEST_FN_NAME,
};

#[derive(Arbitrary, Debug)]
struct Input {
    fn_input: FnInput,
}

#[derive(Arbitrary, Debug)]
struct FnInput {
    msg_pos: u32,
    msg_len: u32,
    vals_pos: u32,
    vals_len: u32,
}

fuzz_target!(|input: Input| {
    // fuzzed code goes here

    let budget = Budget::default();
    let contract_code = wasm_module_with_linear_memory_logging();
    let storage = storage_with_test_contract(contract_code, &budget);
    let host = test_host_with_storage_and_budget(storage, budget);
    host.enable_debug().unwrap();

    let args = test_scvec::<u32>(&[
        input.fn_input.msg_pos,
        input.fn_input.msg_len,
        input.fn_input.vals_pos,
        input.fn_input.vals_len,
    ]);

    let hf = HostFunction::InvokeContract(InvokeContractArgs {
        contract_address: ScAddress::Contract(Hash(TEST_CONTRACT_ID.clone())),
        function_name: ScSymbol(TEST_FN_NAME.try_into().unwrap()),
        args: args.try_into().unwrap(),
    });

    let _res = host.invoke_function(hf).unwrap();

    let cpu = host.as_budget().get_cpu_insns_consumed().unwrap();
    let mem = host.as_budget().get_mem_bytes_consumed().unwrap();
    assert_eq!((cpu, mem), (1077529, 204101));
});
