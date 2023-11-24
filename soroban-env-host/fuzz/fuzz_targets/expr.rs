#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use soroban_env_host::{
    xdr::{HostFunction, InvokeContractArgs, ScErrorCode, ScErrorType, ScSymbol, ScVal, VecM},
    Host,
};
use soroban_synth_wasm::{Emit, Expr};

// We augment the `Expr` we generate with other parameters we'd like the fuzzer to explore.
#[derive(Arbitrary, Debug)]
struct TestCase {
    expr: Expr,
    cpu_budget: u32,
    mem_budget: u32,
}

const TEST_FN_NAME: &'static str = "test";

fuzz_target!(|test: TestCase| {
    let wasm = test.expr.0.as_single_function_wasm_module(TEST_FN_NAME);
    let (host, contract) = Host::test_host_with_wasm_and_enforcing_footprint(&wasm, &[], &[]);
    let contract_address = host.scaddress_from_address(contract).unwrap();
    host.with_budget(|budget| {
        // Mask the budget down to 268m instructions / 256MiB memory so we don't
        // literally run out of time or memory on the machine we're fuzzing on.
        budget.reset_limits(
            test.cpu_budget as u64 & 0x0fff_ffff,
            test.mem_budget as u64 & 0x0ff_ffff,
        )
    })
    .unwrap();

    let args: VecM<ScVal> = vec![].try_into().unwrap();

    let hf = HostFunction::InvokeContract(InvokeContractArgs {
        contract_address,
        function_name: ScSymbol(TEST_FN_NAME.try_into().unwrap()),
        args,
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
