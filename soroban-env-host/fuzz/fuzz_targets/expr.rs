#![no_main]

use std::collections::BTreeMap;

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use soroban_env_host::{
    xdr::{
        AccountId, HostFunction, InvokeContractArgs, PublicKey, ScAddress, ScErrorCode,
        ScErrorType, ScSymbol, ScVal, Uint256,
    },
    Host, StorageType,
};
use soroban_synth_wasm::{Emit, Expr};

// We augment the `Expr` we generate with other parameters we'd like the fuzzer to explore.
#[derive(Arbitrary, Debug)]
struct TestCase {
    cpu_budget: u32,
    mem_budget: u32,
    contract_a_expr: Expr,
    contract_b_expr: Expr,
    data_keys: BTreeMap<u8, (StorageType, bool)>,
    byte_literals: Vec<Vec<u8>>,
    n_signers: u8,
}

const TEST_FN_NAME: &'static str = "test";

impl TestCase {
    fn install_budget(&self, host: &Host) {
        host.with_budget(|budget| {
            // Mask the budget down to 268m instructions / 256MiB memory so we don't
            // literally run out of time or memory on the machine we're fuzzing on;
            // but also mask it _up_ to at least 1m instructions / 1MiB memory so
            // we don't just immediately fail instantiating the VM.
            budget.reset_limits(
                self.cpu_budget as u64 & 0x0fff_ffff | 0x000f_ffff,
                self.mem_budget as u64 & 0x0ff_ffff | 0x000f_ffff,
            )
        })
        .unwrap();
    }
}

fuzz_target!(|test: TestCase| {
    // We generate two contracts A and B. Contract A takes a bunch of typed
    // arguments -- that we generate and supply -- to help it exercise various
    // host functions correctly. Contract B takes exactly one argument so that
    // it's at least _plausible_ that contract A will figure out how to call it
    // correctly (passing 1 Val arg).
    //
    // There is a 32-argument limit to wasm functions imposed for safety in
    // soroban, so we limit the number of keys we generate to 5 read keys and
    // 5 write keys.
    let data_keys = test
        .data_keys
        .iter()
        .map(|(k, v)| (ScVal::U32(*k as u32), v.clone()))
        .take(5)
        .collect::<BTreeMap<_, _>>();
    let mut args_a: Vec<ScVal> = data_keys.keys().cloned().collect();
    let mut arg_tys_a: Vec<&'static str> = args_a.iter().map(|_| "U32Val").collect();
    arg_tys_a.push("AddressObject"); // contract B
    arg_tys_a.push("Symbol"); // test function name
    let n_signers = 3 + (test.n_signers % 4) as usize;
    for _ in 0..n_signers {
        arg_tys_a.push("AddressObject"); // every signer
    }
    let n_byte_literals = test.byte_literals.len().min(3);
    for _ in 0..n_byte_literals {
        arg_tys_a.push("BytesObject");
    }

    let arg_tys_b = vec!["Val"];

    let wasm_a = test
        .contract_a_expr
        .0
        .as_single_function_wasm_module(TEST_FN_NAME, &arg_tys_a);
    let wasm_b = test
        .contract_b_expr
        .0
        .as_single_function_wasm_module(TEST_FN_NAME, &arg_tys_b);
    let (host, contracts, signers) = Host::new_recording_fuzz_host(
        &[wasm_a.as_slice(), wasm_b.as_slice()],
        &data_keys,
        n_signers,
    );

    let contract_address_a = host.scaddress_from_address(contracts[0]).unwrap();

    // Pass contract B's ID and the function name as args so the fuzzer has a
    // chance of figuring out that it can call the test function on contract B.
    args_a.push(ScVal::Address(
        host.scaddress_from_address(contracts[1]).unwrap(),
    ));
    args_a.push(ScVal::Symbol(ScSymbol(TEST_FN_NAME.try_into().unwrap())));

    // Pass all the signer-key account addresses as args so the fuzzer can find
    // them too and use them as arguments to require_auth.
    for signer in signers.iter() {
        let account = AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(
            signer.verifying_key().to_bytes(),
        )));
        args_a.push(ScVal::Address(ScAddress::Account(account)));
    }
    for i in 0..n_byte_literals {
        args_a.push(ScVal::Bytes(
            test.byte_literals[i].clone().try_into().unwrap(),
        ));
    }

    let hf = HostFunction::InvokeContract(InvokeContractArgs {
        contract_address: contract_address_a,
        function_name: ScSymbol(TEST_FN_NAME.try_into().unwrap()),
        args: args_a.try_into().unwrap(),
    });

    // First pass: recording.
    test.install_budget(&host);
    let _ = host.invoke_function(hf.clone());

    // Second pass: enforcing (with synthesized content as needed).
    host.switch_fuzz_host_to_enforcing(&data_keys, &signers);
    test.install_budget(&host);
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
