use expect_test::expect;
use soroban_env_common::{xdr::ScErrorCode, Env, TryFromVal, Val};

use crate::{events::HostEvent, xdr::ScErrorType, Error, Host, HostError, Symbol, Tag};
use soroban_test_wasms::{ADD_I32, ALLOC, INVOKE_CONTRACT, VEC};

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
    println!("total bytes: {:?}", ALLOC.len());
    // let a = 0u32;
    wasmparser::validate(ALLOC).unwrap();

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("sum")?,
        host.test_vec_obj::<u32>(&[])?,
    )?;
    println!("{:?}", res);
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
    dbg!(&events);
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

    // try call -- add will trap, and add_with will trap, but we will get a status
    let status = host.try_call(id0_obj, sym, args)?;
    let code = (ScErrorType::WasmVm, ScErrorCode::InvalidAction);
    let exp: Error = code.into();
    assert_eq!(status.get_payload(), exp.to_val().get_payload());

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
