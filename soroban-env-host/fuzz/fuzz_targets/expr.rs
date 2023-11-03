#![no_main]

use soroban_env_host::{
    budget::Budget,
    xdr::{Hash, HostFunction, InvokeContractArgs, ScAddress, ScErrorCode, ScErrorType, ScSymbol},
};
use soroban_env_host_fuzz::{
    storage_with_test_contract, test_host_with_storage_and_budget, test_scvec, TEST_CONTRACT_ID,
    TEST_FN_NAME,
};
use soroban_synth_wasm::{Emit, Expr};

use libfuzzer_sys::fuzz_target;

fuzz_target!(|expr: Expr| {
    // fuzzed code goes here
    let wasm = expr.0.as_single_function_wasm_module("test");

    let budget = Budget::default();
    let storage = storage_with_test_contract(wasm, &budget);
    let host = test_host_with_storage_and_budget(storage, budget);
    host.enable_debug().unwrap();

    let args = test_scvec::<u32>(&[]);

    let hf = HostFunction::InvokeContract(InvokeContractArgs {
        contract_address: ScAddress::Contract(Hash(TEST_CONTRACT_ID.clone())),
        function_name: ScSymbol(TEST_FN_NAME.try_into().unwrap()),
        args: args.try_into().unwrap(),
    });

    let res = host.invoke_function(hf);

    // Non-internal error-code returns are ok, we are interested in _panics_ and
    // internal errors.
    if let Err(hosterror) = res {
        if hosterror.error.is_code(ScErrorCode::InternalError)
            && !hosterror.error.is_type(ScErrorType::Contract)
        {
            panic!("got internal error: {:?}", hosterror)
        }
    }
});
