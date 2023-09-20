use std::rc::Rc;

use expect_test::expect;
use soroban_env_common::{
    xdr::{self, ScErrorCode},
    Env, EnvBase, TryFromVal, Val,
};

use crate::{
    events::HostEvent, xdr::ScErrorType, ContractFunctionSet, Error, Host, HostError, Symbol, Tag,
};
use soroban_test_wasms::{ADD_I32, ALLOC, ERR, INVOKE_CONTRACT, VEC};

#[test]
fn invoke_single_contract_function() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(ADD_I32);
    let a = 4i32;
    let b = 7i32;
    let c = 0x7fffffff_i32;

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("add")?,
        host.test_vec_obj(&[a, b])?,
    )?;
    assert_eq!(i32::try_from_val(&host, &res)?, a + b);
    // overflow
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("add")?,
        host.test_vec_obj(&[a, c])?,
    );
    let code = (ScErrorType::WasmVm, ScErrorCode::InvalidAction);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn invoke_alloc() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    host.enable_debug()?;
    let contract_id_obj = host.register_test_contract_wasm(ALLOC);
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("sum")?,
        host.test_vec_obj::<u32>(&[128])?,
    )?;
    assert!(res.shallow_eq(&8128_u32.into()));
    let used_bytes = host.budget_cloned().get_mem_bytes_consumed()?;
    // The general pattern of memory growth in this contract will be a sequence
    // of vector-doublings, but these are masked by the fact that we only see
    // the calls that cause the backing vector of wasm linear memory to grow,
    // which happens as the guest vector crosses 64k boundaries (and eventually
    // starts growing in steps larger than 64k itself).
    //
    // So we wind up with a growth-sequence that's a bit irregular: +0x10000,
    // +0x20000, +0x30000, +0x50000, +0x90000. Total is 1 + 2 + 3 + 5 + 9 = 20
    // pages or about 1.3 MiB, plus the initial 17 pages (1.1MiB) plus some more
    // slop from general host machinery allocations, plus allocating a VM once
    // during upload and once during execution we get around 2.5MiB. Call
    // is "less than 4MiB".
    assert!(used_bytes > (128 * 4096));
    assert!(used_bytes < 0x40_0000);
    Ok(())
}

fn invoke_cross_contract(diagnostics: bool) -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    if diagnostics {
        host.set_diagnostic_level(crate::DiagnosticLevel::Debug)?;
    }

    let id_obj = host.register_test_contract_wasm(ADD_I32);
    // prepare arguments
    let sym = Symbol::try_from_small_str("add").unwrap();
    let args = host.test_vec_obj::<i32>(&[1, 2])?;
    let res = host.call(id_obj, sym, args)?;
    assert_eq!(res.get_tag(), Tag::I32Val);
    let i: i32 = res.try_into()?;
    assert_eq!(i, 3);
    Ok(())
}

#[test]
fn invoke_cross_contract_without_diagnostics() -> Result<(), HostError> {
    invoke_cross_contract(false)
}

#[test]
fn invoke_cross_contract_with_diagnostics() -> Result<(), HostError> {
    invoke_cross_contract(true)
}

#[test]
fn invoke_cross_contract_with_err() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    host.enable_debug()?;
    let id_obj = host.register_test_contract_wasm(VEC);
    // prepare arguments
    let sym = Symbol::try_from_small_str("vec_err").unwrap();
    let args = host.test_vec_obj::<u32>(&[1])?;

    // try_call
    let sv = host.try_call(id_obj, sym, args)?;
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    let exp_st: Error = code.into();
    assert_eq!(sv.get_payload(), exp_st.to_val().get_payload());

    let events = host.get_events()?.0;
    assert_eq!(events.len(), 5);
    let last_event: &HostEvent = events.last().unwrap();
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[
        r#"[Diagnostic Event] topics:[error, Error(Object, IndexBounds)], data:["contract try_call failed", vec_err, [1]]"#
    ]];
    let actual = format!("{}", last_event);
    expected.assert_eq(&actual);

    // call
    let res = host.call(id_obj, sym, args);
    assert!(HostError::result_matches_err(res, code));

    let events = host.get_events()?.0;
    assert_eq!(events.len(), 10);
    let last_event = events.last().unwrap();
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[
        r#"[Diagnostic Event] topics:[error, Error(Object, IndexBounds)], data:["contract call failed", vec_err, [1]]"#
    ]];
    let actual = format!("{}", last_event);
    expected.assert_eq(&actual);

    Ok(())
}

#[test]
fn invoke_cross_contract_indirect() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let id0_obj = host.register_test_contract_wasm(INVOKE_CONTRACT);
    let id1_obj = host.register_test_contract_wasm(ADD_I32);
    // prepare arguments
    let sym = Symbol::try_from_small_str("add_with").unwrap();
    let args = host.test_vec_obj::<i32>(&[5, 6])?;
    let args = host.vec_push_back(args, id1_obj.to_val())?;
    // try call
    let val = host.call(id0_obj, sym, args)?;
    let exp: Val = 11i32.into();
    assert_eq!(val.get_payload(), exp.get_payload());
    Ok(())
}

#[test]
fn invoke_cross_contract_indirect_err() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    host.enable_debug()?;
    let id0_obj = host.register_test_contract_wasm(INVOKE_CONTRACT);
    let sym = Symbol::try_from_small_str("add_with").unwrap();
    let args = host.test_vec_obj::<i32>(&[i32::MAX, 1])?;
    let args = host.vec_push_back(args, host.bytes_new()?.to_val())?;

    // try call -- add will trap, and add_with will trap, but we will get an error
    let error = host.try_call(id0_obj, sym, args)?;
    let code = (ScErrorType::WasmVm, ScErrorCode::InvalidAction);
    let exp: Error = code.into();
    assert_eq!(error.get_payload(), exp.to_val().get_payload());

    let events = host.get_events()?.0;
    assert_eq!(events.len(), 2);
    let last_event = events.last().unwrap();
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[
        r#"[Diagnostic Event] topics:[error, Error(WasmVm, InvalidAction)], data:["contract try_call failed", add_with, [2147483647, 1, Bytes()]]"#
    ]];
    let actual = format!("{}", last_event);
    expected.assert_eq(&actual);

    // call
    let res = host.call(id0_obj, sym, args);
    assert!(HostError::result_matches_err(res, code));

    let events = host.get_events()?.0;
    assert_eq!(events.len(), 4);
    let last_event = events.last().unwrap();
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[
        r#"[Diagnostic Event] topics:[error, Error(WasmVm, InvalidAction)], data:["contract call failed", add_with, [2147483647, 1, Bytes()]]"#
    ]];
    let actual = format!("{}", last_event);
    expected.assert_eq(&actual);

    Ok(())
}

#[test]
fn invoke_contract_with_reentry() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let id0_obj = host.register_test_contract_wasm(INVOKE_CONTRACT);
    // prepare arguments
    let sym = Symbol::try_from_small_str("add_with").unwrap();
    let args = host.test_vec_obj::<i32>(&[i32::MAX, 1])?;
    let args = host.vec_push_back(args, id0_obj.into())?; // trying to call its own `add` function

    // call -- add will trap, and add_with will trap, we will get an Err(Error)
    let call_res = host.call(id0_obj, sym, args);
    // try_call -- add will trap, and add_with will trap, but we will get an Ok(Error)
    let try_call_val = host.try_call(id0_obj, sym, args)?;

    let code = (ScErrorType::Context, ScErrorCode::InvalidAction);
    let err: Error = code.into();
    assert!(HostError::result_matches_err(call_res, code));
    let try_call_err = Error::try_from(try_call_val)?;
    assert_eq!(try_call_err, err);

    Ok(())
}

struct ReturnContractError;
impl ReturnContractError {
    const ERR: Error = Error::from_contract_error(12345);
}
impl ContractFunctionSet for ReturnContractError {
    fn call(&self, _func: &Symbol, _host: &Host, _args: &[Val]) -> Option<Val> {
        Some(Self::ERR.into())
    }
}

#[test]
fn native_invoke_return_err_variants() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let addr = host.add_host_object(xdr::ScAddress::Contract(xdr::Hash([0; 32])))?;
    host.register_test_contract(addr, Rc::new(ReturnContractError))?;

    let sym = Symbol::try_from_small_str("go")?;
    let args = host.vec_new()?;
    let err = ReturnContractError::ERR;

    // We want a call to return `Err(Error)` not `Ok(Error)`
    let call_res = host.call(addr, sym, args);
    assert!(HostError::result_matches_err(call_res, err));

    // We want a try_call to return `Ok(Error)`
    let try_call_val = host.try_call(addr, sym, args)?;
    assert!(try_call_val.shallow_eq(&err.into()));

    Ok(())
}

#[test]
fn wasm_invoke_return_err_variants() -> Result<(), HostError> {
    // Here we test several variants of returning-a-Val-that-is-an-Error
    // causes Err(Error) from call and Ok(Error) from try_call.
    for fname in ["err_eek", "err_err", "ok_err", "ok_val_err", "err", "val"].iter() {
        let expected_err = Error::from_contract_error(12345);
        let host = Host::test_host_with_recording_footprint();
        host.enable_debug()?;
        let addr = host.register_test_contract_wasm(ERR);
        let sym = Symbol::try_from_val(&host, fname)?;
        let args = host.vec_new_from_slice(&[])?;
        let call_res = host.call(addr, sym, args);
        let try_call_res = host.try_call(addr, sym, args);
        if let Err(got_err) = call_res {
            assert_eq!(got_err.error, expected_err)
        } else {
            dbg!(&call_res);
            panic!("got Ok when expected Err from call({})", fname)
        }
        if let Ok(got_err) = try_call_res {
            assert!(got_err.shallow_eq(&expected_err.to_val()))
        } else {
            dbg!(&try_call_res);
            panic!("got Err when expected Ok from try_call({})", fname)
        }
    }
    Ok(())
}
