use crate::{
    budget::AsBudget, builtin_contracts::base_types::Address, test::observe::ObservedHost,
    BytesObject, Env, EnvBase, Host, HostError, Symbol, TryFromVal, TryIntoVal, Val,
    DEFAULT_HOST_DEPTH_LIMIT, DEFAULT_XDR_RW_LIMITS,
};
use soroban_builtin_sdk_macros::contracttype;
use soroban_test_wasms::RECURSIVE_ACCOUNT_CONTRACT;

use crate::xdr::{
    InvokeContractArgs, ScErrorType, ScVal, SorobanAddressCredentials, SorobanAuthorizationEntry,
    SorobanAuthorizedFunction, SorobanAuthorizedInvocation, SorobanCredentials, WriteXdr,
};

#[contracttype]
enum RecursiveAccountSignature {
    RedirectAddress(Address),
    SerializeValue,
    DeserializeValue(BytesObject),
}

fn create_recursive_serialized_scval(depth: u32) -> Vec<u8> {
    let mut val = ScVal::Vec(Some(vec![ScVal::Void].try_into().unwrap()));
    for _ in 0..depth {
        val = ScVal::Vec(Some(vec![val.clone()].try_into().unwrap()));
    }
    let mut limits = DEFAULT_XDR_RW_LIMITS;
    // Set depth much higher than default to be able to test deeper inputs
    // than host can decode.
    limits.depth *= 10;
    val.to_xdr(limits).unwrap()
}

fn run_deep_host_stack_test(
    contract_call_depth: u32,
    serialization_depth: u32,
    deserialization_depth: u32,
    function_name: &'static str,
) -> Result<Val, HostError> {
    let host = ObservedHost::new(function_name, Host::test_host_with_recording_footprint());

    let mut contracts = vec![];
    // Reset budget to unlimited. While it's likely that the malicious
    // contract would run out of budget before reaching the call stack
    // depth, this simply accounts for the potential future host
    // cost optimizations and contract implementation optimizations.
    host.as_budget().reset_unlimited().unwrap();

    // Contract instance setup:
    // Contract `0` doesn't perform auth and is only used for the
    // root `call` function.
    // Contracts `1..CONTRACT_CALL_DEPTH - 2` recursively call
    // the next contract in the sequence.
    // Contract `CONTRACT_CALL_DEPTH - 1` serializes nested XDR value
    // of depth `SERIALIZATION_DEPTH`.
    for _ in 0..contract_call_depth {
        contracts.push(
            Address::try_from_val(
                &*host,
                &host.register_test_contract_wasm(RECURSIVE_ACCOUNT_CONTRACT),
            )
            .unwrap(),
        );
    }

    let mut auth_entries = vec![];
    for i in 1..contract_call_depth as usize {
        let signature = if i < (contract_call_depth - 1) as usize {
            RecursiveAccountSignature::RedirectAddress(contracts[i + 1].clone())
        } else {
            if deserialization_depth > 0 {
                assert_eq!(serialization_depth, 0);
                RecursiveAccountSignature::DeserializeValue(
                    host.bytes_new_from_slice(
                        create_recursive_serialized_scval(deserialization_depth).as_slice(),
                    )
                    .unwrap(),
                )
            } else {
                RecursiveAccountSignature::SerializeValue
            }
        };
        let signature_val: Val = signature.try_into_val(&*host).unwrap();
        let credentials = SorobanAddressCredentials {
            address: contracts[i].to_sc_address().unwrap(),
            nonce: 0,
            signature_expiration_ledger: 1000,
            signature: signature_val.try_into_val(&*host).unwrap(),
        };
        let function_name = if i == 1 { "call" } else { "__check_auth" };
        let root_invocation = SorobanAuthorizedInvocation {
            function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                contract_address: contracts[i - 1].to_sc_address().unwrap(),
                function_name: function_name.try_into().unwrap(),
                args: Default::default(),
            }),
            sub_invocations: Default::default(),
        };
        auth_entries.push(SorobanAuthorizationEntry {
            credentials: SorobanCredentials::Address(credentials),
            root_invocation,
        });
    }
    if serialization_depth > 0 {
        host.call(
            contracts.last().unwrap().as_object(),
            Symbol::try_from_small_str("set_depth").unwrap(),
            test_vec![&*host, serialization_depth].into(),
        )
        .unwrap();
    }

    host.set_authorization_entries(auth_entries).unwrap();
    // Note, that we only are interested in the end result of the test;
    // the setup shouldn't fail and doesn't need to be verified.
    host.call(
        contracts[0].as_object(),
        Symbol::try_from_small_str("call").unwrap(),
        test_vec![&*host, contracts[1]].into(),
    )
}

#[test]
fn test_deep_stack_call_succeeds_near_limit() {
    // The serialized object has depth of `serialization_depth + 1`,
    // thus the maximum serializable value needs
    // `serialization_depth == DEFAULT_HOST_DEPTH_LIMIT - 1`.
    let res = run_deep_host_stack_test(
        DEFAULT_HOST_DEPTH_LIMIT,
        DEFAULT_HOST_DEPTH_LIMIT - 1,
        0,
        function_name!(),
    );
    assert!(res.is_ok());
}

#[test]
fn test_deep_stack_call_fails_when_contract_call_depth_exceeded() {
    let res = run_deep_host_stack_test(DEFAULT_HOST_DEPTH_LIMIT + 1, 0, 0, function_name!());
    assert!(res.is_err());
    let err = res.err().unwrap().error;
    // We shouldn't run out of budget here, so the error would just
    // be decorated as auth error.
    assert!(err.is_type(ScErrorType::Auth));
}

#[test]
fn test_deep_stack_call_fails_when_serialization_depth_exceeded() {
    let res = run_deep_host_stack_test(2, DEFAULT_HOST_DEPTH_LIMIT, 0, function_name!());
    assert!(res.is_err());
    let err = res.err().unwrap().error;
    // We shouldn't run out of budget here, so the error would just
    // be decorated as auth error.
    assert!(err.is_type(ScErrorType::Auth));
}

#[test]
fn test_deep_stack_call_succeeds_near_limit_with_xdr_deserialization() {
    let res = run_deep_host_stack_test(
        DEFAULT_HOST_DEPTH_LIMIT,
        0,
        DEFAULT_HOST_DEPTH_LIMIT - 2,
        function_name!(),
    );
    assert!(res.is_ok());
}

#[test]
fn test_deep_stack_call_fails_xdr_deserialization_exceeding_host_object_depth() {
    let res = run_deep_host_stack_test(
        DEFAULT_HOST_DEPTH_LIMIT,
        0,
        DEFAULT_HOST_DEPTH_LIMIT - 1,
        function_name!(),
    );
    assert!(res.is_err());
    let err = res.err().unwrap().error;
    // We shouldn't run out of budget here, so the error would just
    // be decorated as auth error.
    assert!(err.is_type(ScErrorType::Auth));
}

#[test]
fn test_deep_stack_call_fails_with_deep_xdr_deserialization() {
    let res = run_deep_host_stack_test(
        DEFAULT_HOST_DEPTH_LIMIT,
        0,
        DEFAULT_XDR_RW_LIMITS.depth,
        function_name!(),
    );
    assert!(res.is_err());
    let err = res.err().unwrap().error;
    // We shouldn't run out of budget here, so the error would just
    // be decorated as auth error.
    assert!(err.is_type(ScErrorType::Auth));
}

#[test]
fn test_deep_stack_call_fails_with_too_deep_xdr_deserialization() {
    let res = run_deep_host_stack_test(
        DEFAULT_HOST_DEPTH_LIMIT,
        0,
        DEFAULT_XDR_RW_LIMITS.depth * 2,
        function_name!(),
    );
    assert!(res.is_err());
    let err = res.err().unwrap().error;
    // We shouldn't run out of budget here, so the error would just
    // be decorated as auth error.
    assert!(err.is_type(ScErrorType::Auth));
}
