#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use soroban_env_host::{
    xdr::{HostFunction, InvokeContractArgs, ScErrorCode, ScErrorType, ScSymbol, ScVal, Validate},
    Host, StorageType, Val
};
use soroban_synth_wasm::{Emit, Expr};

// We augment the `Expr` we generate with other parameters we'd like the fuzzer to explore.
#[derive(Arbitrary, Debug)]
struct TestCase {
    contract_a_expr: Expr,
    contract_b_expr: Expr,
    read_keys: Vec<(ScVal, StorageType)>,
    write_keys: Vec<(ScVal, StorageType)>,
    cpu_budget: u32,
    mem_budget: u32,
}

fn is_ok(s: &ScVal) -> bool{
    Val::can_represent_scval(s) && s.validate().is_ok()
}

const TEST_FN_NAME: &'static str = "test";

fuzz_target!(|test: TestCase| {
    // We generate two contracts A and B. Contract A takes a bunch of typed
    // arguments -- that we generate and supply -- to help it exercise various
    // host functions correctly. Contract B takes exactly one argument so that
    // it's at least _plausible_ that contract A will figure out how to call it
    // correctly (passing 1 Val arg).
    let mut arg_tys_a: Vec<&'static str> = vec!["AddressObject", "AddressObject"];
    let mut args_a: Vec<ScVal> = vec![];
    let read_keys = test.read_keys.iter().filter(|(k, _)| is_ok(k)).cloned().collect::<Vec<_>>();
    let write_keys = test.write_keys.iter().filter(|(k, _)| is_ok(k)).cloned().collect::<Vec<_>>();
    for arg in read_keys.iter().chain(write_keys.iter()) {
        use soroban_env_host::xdr::ScValType;
        let ty = match arg.0.discriminant() {
            ScValType::Bool => "Bool",
            ScValType::Void => "Void",
            ScValType::Error => "Error",
            ScValType::U32 => "U32Val",
            ScValType::I32 => "I32Val",
            ScValType::U64 =>  "U64Val",
            ScValType::I64 =>  "I64Val",
            ScValType::Timepoint => "TimepointVal",
            ScValType::Duration =>  "DurationVal",
            ScValType::U128 =>  "U128Val",
            ScValType::I128 =>  "I128Val",
            ScValType::U256 =>  "U256Val",
            ScValType::I256 =>  "I256Val",
            ScValType::Bytes =>  "BytesObject",
            ScValType::String =>  "StringObject",
            ScValType::Symbol =>  "Symbol",
            ScValType::Vec =>  "VecObject",
            ScValType::Map =>  "MapObject",
            ScValType::Address =>  "AddressObject",
            ScValType::ContractInstance |
            ScValType::LedgerKeyContractInstance |
            ScValType::LedgerKeyNonce => unreachable!()
        };
        arg_tys_a.push(ty);
        args_a.push(arg.0.clone());
    }
    let args_b = vec!["Val"];
    let wasm_a = test.contract_a_expr.0.as_single_function_wasm_module(TEST_FN_NAME, &arg_tys_a);
    let wasm_b = test.contract_b_expr.0.as_single_function_wasm_module(TEST_FN_NAME, &args_b);
    let (host, contracts) = Host::test_host_with_wasms_and_enforcing_footprint(&[wasm_a.as_slice(), wasm_b.as_slice()], &read_keys, &write_keys);
    for a in contracts.iter() {
        args_a.insert(0, ScVal::Address(host.scaddress_from_address(*a).unwrap()));
    }
    let contract_address_a = host.scaddress_from_address(contracts[0]).unwrap();
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
