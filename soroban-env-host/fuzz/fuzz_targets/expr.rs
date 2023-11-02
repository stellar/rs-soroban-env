#![no_main]

use soroban_env_host::{
    xdr::{ScErrorCode, ScErrorType},
    Env, Host, Symbol,
};
use soroban_synth_wasm::{Emit, Expr};

use libfuzzer_sys::fuzz_target;
 
fuzz_target!(|expr: Expr| {
    // fuzzed code goes here
    let wasm = expr.0.as_single_function_wasm_module("test");
    let host = Host::test_host_with_recording_footprint();
    host.enable_debug().unwrap();
    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("test").unwrap(),
        host.test_vec_obj::<u32>(&[]).unwrap(),
    );
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
