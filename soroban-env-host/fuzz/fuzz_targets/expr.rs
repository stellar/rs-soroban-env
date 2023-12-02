#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use soroban_env_host::{
    xdr::{HostFunction, InvokeContractArgs, ScErrorCode, ScErrorType, ScSymbol, ScVal},
    Host, StorageType,
};
use soroban_synth_wasm::{Emit, Expr};

// We augment the `Expr` we generate with other parameters we'd like the fuzzer to explore.
#[derive(Arbitrary, Debug)]
struct TestCase {
    contract_a_expr: Expr,
    contract_b_expr: Expr,
    // we use u32 vals because they are always valid and we don't want to
    // bother generating and rejecting complex values, we can generate them
    // _inside_ the contract more constructively.
    read_keys: Vec<(u32, StorageType)>,
    write_keys: Vec<(u32, StorageType)>,
    cpu_budget: u32,
    mem_budget: u32,
}

const TEST_FN_NAME: &'static str = "test";

fuzz_target!(|test: TestCase| {
    // We generate two contracts A and B. Contract A takes a bunch of typed
    // arguments -- that we generate and supply -- to help it exercise various
    // host functions correctly. Contract B takes exactly one argument so that
    // it's at least _plausible_ that contract A will figure out how to call it
    // correctly (passing 1 Val arg).
    let read_keys: Vec<(ScVal, StorageType)> = test
        .read_keys
        .iter()
        .map(|(k, ty)| (ScVal::U32(*k), *ty))
        .collect();
    let write_keys: Vec<(ScVal, StorageType)> = test
        .write_keys
        .iter()
        .map(|(k, ty)| (ScVal::U32(*k), *ty))
        .collect();

    let mut args_a: Vec<ScVal> = read_keys
        .iter()
        .chain(write_keys.iter())
        .map(|(k, _)| k.clone())
        .collect();
    let mut arg_tys_a: Vec<&'static str> = args_a.iter().map(|_| "U32Val").collect();
    arg_tys_a.push("AddressObject"); // contract B
    arg_tys_a.push("Symbol"); // test function name

    let arg_tys_b = vec!["Val"];

    let wasm_a = test
        .contract_a_expr
        .0
        .as_single_function_wasm_module(TEST_FN_NAME, &arg_tys_a);
    let wasm_b = test
        .contract_b_expr
        .0
        .as_single_function_wasm_module(TEST_FN_NAME, &arg_tys_b);
    let (host, contracts) = Host::test_host_with_wasms_and_enforcing_footprint(
        &[wasm_a.as_slice(), wasm_b.as_slice()],
        &read_keys,
        &write_keys,
    );

    let contract_address_a = host.scaddress_from_address(contracts[0]).unwrap();

    // Pass contract B's ID and the function name as args so the fuzzer has a chance of figuring
    // out that it can call the test function on contract B.
    args_a.push(ScVal::Address(
        host.scaddress_from_address(contracts[1]).unwrap(),
    ));
    args_a.push(ScVal::Symbol(ScSymbol(TEST_FN_NAME.try_into().unwrap())));

    host.with_budget(|budget| {
        // Mask the budget down to 268m instructions / 256MiB memory so we don't
        // literally run out of time or memory on the machine we're fuzzing on;
        // but also mask it _up_ to at least 1m instructions / 1MiB memory so
        // we don't just immediately fail instantiating the VM.
        budget.reset_limits(
            test.cpu_budget as u64 & 0x0fff_ffff | 0x000f_ffff,
            test.mem_budget as u64 & 0x0ff_ffff | 0x000f_ffff,
        )
    })
    .unwrap();

    let hf = HostFunction::InvokeContract(InvokeContractArgs {
        contract_address: contract_address_a,
        function_name: ScSymbol(TEST_FN_NAME.try_into().unwrap()),
        args: args_a.try_into().unwrap(),
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
