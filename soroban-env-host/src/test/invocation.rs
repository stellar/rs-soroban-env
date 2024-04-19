use std::{cmp::Ordering, rc::Rc};

use expect_test::expect;
use soroban_env_common::{
    xdr::{self, ContractCostType, ScError, ScErrorCode},
    Compare, Env, EnvBase, TryFromVal, TryIntoVal, Val,
};

use crate::{
    budget::AsBudget, events::HostEvent, test::observe::ObservedHost, xdr::ScErrorType,
    ContractFunctionSet, Error, Host, HostError, Symbol, Tag,
};
use soroban_test_wasms::{ADD_I32, ALLOC, ERR, INVOKE_CONTRACT, VEC};

#[test]
fn invoke_single_contract_function() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id_obj = host.register_test_contract_wasm(ADD_I32);
    let a = 4i32;
    let b = 7i32;
    let c = 0x7fffffff_i32;

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("add")?,
        host.test_vec_obj(&[a, b])?,
    )?;
    assert_eq!(i32::try_from_val(&*host, &res)?, a + b);
    // overflow
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("add")?,
        host.test_vec_obj(&[a, c])?,
    );
    let code = (ScErrorType::WasmVm, ScErrorCode::InvalidAction);

    eprintln!(
        "time ellapsed in nano-seconds for VmInstantiation: {}",
        host.as_budget()
            .get_time(ContractCostType::VmInstantiation)?
    );

    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn invoke_alloc() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
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
    // is "less than 5MiB".
    assert!(used_bytes > (128 * 4096));
    assert!(used_bytes < 0x50_0000);
    Ok(())
}

fn invoke_cross_contract(diagnostics: bool, testname: &'static str) -> Result<(), HostError> {
    let host = ObservedHost::new(testname, Host::test_host_with_recording_footprint());
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
    invoke_cross_contract(false, function_name!())
}

#[test]
fn invoke_cross_contract_with_diagnostics() -> Result<(), HostError> {
    invoke_cross_contract(true, function_name!())
}

#[test]
fn invoke_cross_contract_with_err() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.enable_debug()?;
    let id_obj = host.register_test_contract_wasm(VEC);
    // prepare arguments
    let sym = Symbol::try_from_small_str("vec_err").unwrap();
    let args = host.test_vec_obj::<u32>(&[1])?;

    // try_call
    let sv = host.try_call(id_obj, sym, args)?;
    let generic_host_error: Error = (ScErrorType::Context, ScErrorCode::InvalidAction).into();
    assert_eq!(sv.get_payload(), generic_host_error.to_val().get_payload());

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
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Object, ScErrorCode::IndexBounds)
    ));

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
    let host = observe_host!(Host::test_host_with_recording_footprint());
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
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.enable_debug()?;
    let id0_obj = host.register_test_contract_wasm(INVOKE_CONTRACT);
    let sym = Symbol::try_from_small_str("add_with").unwrap();
    let args = host.test_vec_obj::<i32>(&[i32::MAX, 1])?;
    let args = host.vec_push_back(args, host.bytes_new()?.to_val())?;

    // try call -- add will trap, and add_with will trap, but we will get an error
    let error = host.try_call(id0_obj, sym, args)?;
    let generic_host_error: Error = (ScErrorType::Context, ScErrorCode::InvalidAction).into();
    assert_eq!(
        error.get_payload(),
        generic_host_error.to_val().get_payload()
    );

    let events = host.get_events()?.0;
    assert_eq!(events.len(), 3);
    let last_event = events.last().unwrap();
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[
        r#"[Diagnostic Event] topics:[error, Error(WasmVm, InvalidAction)], data:["contract try_call failed", add_with, [2147483647, 1, Bytes()]]"#
    ]];
    let actual = format!("{}", last_event);
    expected.assert_eq(&actual);

    // call
    let res = host.call(id0_obj, sym, args);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));

    let events = host.get_events()?.0;
    assert_eq!(events.len(), 6);
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
fn contract_failure_with_debug_on_off_affects_no_metering() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let id0_obj = host.register_test_contract_wasm(INVOKE_CONTRACT);
    let sym = Symbol::try_from_small_str("add_with").unwrap();
    let args = host.test_vec_obj::<i32>(&[i32::MAX, 1])?;
    let args = host.vec_push_back(args, host.bytes_new()?.to_val())?;

    let invoke_cross_contract_indirect_with_err = || -> Result<(u64, u64, u64, u64), HostError> {
        // try call -- add will trap, and add_with will trap, but we will get an error
        host.rebuild_module_cache()?;
        host.as_budget().reset_default()?;
        let res = host.try_call(id0_obj, sym, args);
        HostError::result_matches_err(
            res,
            Error::from_type_and_code(ScErrorType::Context, ScErrorCode::InvalidAction),
        );
        Ok((
            host.as_budget().get_cpu_insns_consumed()?,
            host.as_budget().get_mem_bytes_consumed()?,
            host.as_budget().get_shadow_cpu_insns_consumed()?,
            host.as_budget().get_shadow_mem_bytes_consumed()?,
        ))
    };

    let (off_cpu, off_mem, off_shadow_cpu, off_shadow_mem) =
        invoke_cross_contract_indirect_with_err()?;

    host.enable_debug()?;

    let (on_cpu, on_mem, on_shadow_cpu, on_shadow_mem) = invoke_cross_contract_indirect_with_err()?;

    assert_eq!((on_cpu, on_mem), (off_cpu, off_mem));
    assert_ne!(
        (on_shadow_cpu, on_shadow_mem),
        (off_shadow_cpu, off_shadow_mem)
    );

    Ok(())
}

#[test]
fn invoke_contract_with_reentry() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
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
    let host = observe_host!(Host::test_host_with_recording_footprint());
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
    let expected_err = Error::from_contract_error(12345);
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.enable_debug()?;
    let addr = host.register_test_contract_wasm(ERR);
    for fname in ["err_eek", "err_err", "ok_err", "ok_val_err", "err", "val"].iter() {
        let sym = Symbol::try_from_val(&*host, fname)?;
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

#[test]
fn error_spoof_rejected() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.enable_debug()?;
    let addr = host.register_test_contract_wasm(ERR);
    let sym = Symbol::try_from_small_str("spoof")?;
    let args = host.vec_new_from_slice(&[])?;

    // This will try to return Ok(Error(Context,InternalError)) and we'll
    // reject it and turn it into Err(Context,InvalidAction)
    let call_res = host.call(addr, sym, args);
    assert!(HostError::result_matches_err(
        call_res,
        Error::from_type_and_code(ScErrorType::Context, ScErrorCode::InvalidAction),
    ));

    // This will return Ok(Error(Context, InvalidAction)) because while the
    // contract tried to break the rules, yielding Err(Error(Context,
    // InvalidAction)), we used try_call and this is a recoverable type of
    // error.
    let try_call_res = host.try_call(addr, sym, args);
    let Ok(val) = try_call_res else {
        panic!("got Err, expected Ok: {:?}", try_call_res)
    };
    let Ok(err) = Error::try_from(val) else {
        panic!("got non-Error: {:?}", val)
    };
    assert!(err.is_type(ScErrorType::Context) && err.is_code(ScErrorCode::InvalidAction));
    Ok(())
}

#[test]
fn unrecoverable_error_with_cross_contract_try_call() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id_obj: soroban_env_common::AddressObject =
        host.register_test_contract_wasm(ADD_I32);
    let invoke_contract_id_obj = host.register_test_contract_wasm(INVOKE_CONTRACT);

    let _ = host.clone().test_budget(5789, 10_048_576).enable_model(
        ContractCostType::WasmInsnExec,
        6,
        0,
        0,
        0,
    );

    let a = 4i32;
    let b = 7i32;

    let res = host.try_call(
        invoke_contract_id_obj,
        Symbol::try_from_val(&*host, &"add_with_try")?,
        test_vec![&*host, a, b, contract_id_obj].into(),
    );

    // Running out of budget is a unrecoverable error
    assert!(res.is_err());
    let err = res.err().unwrap().error;

    assert!(err.is_type(ScErrorType::Budget));
    assert!(err.is_code(ScErrorCode::ExceededLimit));

    Ok(())
}

#[test]
fn unrecoverable_error_with_try_call() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id_obj = host.register_test_contract_wasm(ADD_I32);

    let _ = host.clone().test_budget(2015, 1_048_576).enable_model(
        ContractCostType::WasmInsnExec,
        6,
        0,
        0,
        0,
    );

    let a = 4i32;
    let b = 7i32;
    let res = host.try_call(
        contract_id_obj,
        Symbol::try_from_small_str("add")?,
        host.test_vec_obj(&[a, b])?,
    );

    // Running out of budget is a unrecoverable error
    assert!(res.is_err());
    let err = res.err().unwrap().error;

    assert!(err.is_type(ScErrorType::Budget));
    assert!(err.is_code(ScErrorCode::ExceededLimit));
    Ok(())
}

#[test]
fn host_function_error() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.enable_debug()?;
    let addr = host.register_test_contract_wasm(ERR);

    let sym = Symbol::try_from_val(&*host, &"missing_wasm")?;
    let args = host.vec_new_from_slice(&[])?;
    let call_err = host.call(addr, sym, args).err().unwrap();
    let try_call_res = host.try_call(addr, sym, args)?;

    assert!(call_err.error.is_type(ScErrorType::Storage));
    assert!(call_err.error.is_code(ScErrorCode::MissingValue));

    // Storage(MissingValue) is a recoverable error
    // It gets narrowed to a Context error so granular host
    // error codes aren't hashed in to the blockchain, making the
    // protocol less flexible in the future
    let e = Error::from_scerror(ScError::Context(ScErrorCode::InvalidAction));
    assert_eq!(
        (*host).compare(&e.to_val(), &try_call_res)?,
        Ordering::Equal
    );
    Ok(())
}

#[test]
fn guest_error() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.enable_debug()?;
    let addr = host.register_test_contract_wasm(ERR);

    let sym = Symbol::try_from_val(&*host, &"divide")?;
    let args = host.vec_new_from_slice(&[0.into()])?;
    let call_err = host.call(addr, sym, args).err().unwrap();
    let try_call_res = host.try_call(addr, sym, args)?;

    assert!(call_err.error.is_type(ScErrorType::WasmVm));
    assert!(call_err.error.is_code(ScErrorCode::InvalidAction));

    // WasmVm(InvalidAction) is a recoverable error
    // It gets narrowed to a Context error so granular host
    // error codes aren't hashed in to the blockchain, making the
    // protocol less flexible in the future
    let e = Error::from_scerror(ScError::Context(ScErrorCode::InvalidAction));
    assert_eq!(
        (*host).compare(&e.to_val(), &try_call_res)?,
        Ordering::Equal
    );
    Ok(())
}
