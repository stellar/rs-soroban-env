use soroban_env_common::{
    xdr::{ScHostContextErrorCode, ScVmErrorCode},
    RawVal,
};

use crate::{
    events::{DebugArg, Event},
    vm::Vm,
    xdr::{Hash, ScHostObjErrorCode, ScStatusType, ScVal, ScVec},
    Env, Host, HostError, Status, Symbol, Tag,
};
use soroban_test_wasms::{ADD_I32, INVOKE_CONTRACT, VEC};

#[test]
fn invoke_single_contract_function() -> Result<(), HostError> {
    use soroban_env_common::xdr::ScVmErrorCode;

    let host = Host::default();
    let id: Hash = [0; 32].into();
    let vm = Vm::new(&host, id, ADD_I32)?;
    let a = 4i32;
    let b = 7i32;
    let c = 0x7fffffff_i32;
    let scvec0: ScVec = host.test_scvec::<i32>(&[a, b])?;
    let res = vm.invoke_function(&host, "add", &scvec0)?;
    match res {
        ScVal::I32(v) => assert_eq!(v, a + b),
        _ => panic!("Wrong result type"),
    }
    // overflow
    let scvec0: ScVec = host.test_scvec::<i32>(&[a, c])?;
    let res = vm.invoke_function(&host, "add", &scvec0);
    let code = ScVmErrorCode::TrapUnreachable;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

fn invoke_cross_contract(diagnostics: bool) -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    if diagnostics {
        host.set_diagnostic_level(crate::DiagnosticLevel::Debug);
    }

    let id_obj = host.register_test_contract_wasm(ADD_I32)?;
    // prepare arguments
    let sym = Symbol::try_from_small_str("add").unwrap();
    let args = host.test_vec_obj::<i32>(&[1, 2])?;
    let res = host.call(id_obj, sym, args)?;
    assert!(res.is::<i32>());
    assert!(res.get_tag() == Tag::I32Val);
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
    let id_obj = host.register_test_contract_wasm(VEC)?;
    // prepare arguments
    let sym = Symbol::try_from_small_str("vec_err").unwrap();
    let args = host.test_vec_obj::<u32>(&[1])?;

    // try_call
    let sv = host.try_call(id_obj, sym, args)?;
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    let exp_st: Status = code.into();
    assert_eq!(sv.get_payload(), exp_st.to_raw().get_payload());

    let events = host.get_events()?.0;
    assert_eq!(events.len(), 4);
    let last_event = events.last().unwrap();
    match &last_event.event {
        Event::Debug(de) => {
            assert_eq!(
                de.msg,
                Some("contract call invocation resulted in error {}".into())
            );
            assert_eq!(de.args.len(), 1);
            if let DebugArg::Val(rv) = de.args[0] {
                let status: Status = rv.try_into()?;
                assert_eq!(
                    status,
                    Status::from_type_and_code(
                        ScStatusType::HostObjectError,
                        ScHostObjErrorCode::VecIndexOutOfBound as u32,
                    )
                );
            } else {
                panic!("got: {:?}, want debug arg containing Val", de);
            }
        }
        _ => {
            panic!("got: {:?}, want debug event", last_event);
        }
    };

    // call
    let res = host.call(id_obj, sym, args);
    assert!(HostError::result_matches_err_status(res, code));

    let events = host.get_events()?.0;
    assert_eq!(events.len(), 8);
    let last_event = events.last().unwrap();
    match &last_event.event {
        Event::Debug(de) => {
            assert_eq!(
                de.msg,
                Some("contract call invocation resulted in error {}".into())
            );
            assert_eq!(de.args.len(), 1);
            if let DebugArg::Val(rv) = de.args[0] {
                let status: Status = rv.try_into()?;
                assert_eq!(
                    status,
                    Status::from_type_and_code(
                        ScStatusType::HostObjectError,
                        ScHostObjErrorCode::VecIndexOutOfBound as u32,
                    )
                );
            } else {
                panic!("got: {:?}, want debug arg containing Val", de);
            }
        }
        _ => {
            panic!("got: {:?}, want debug event", last_event);
        }
    };

    Ok(())
}

#[test]
fn invoke_cross_contract_indirect() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let id0_obj = host.register_test_contract_wasm(INVOKE_CONTRACT)?;
    let id1_obj = host.register_test_contract_wasm(ADD_I32)?;
    // prepare arguments
    let sym = Symbol::try_from_small_str("add_with").unwrap();
    let args = host.test_vec_obj::<i32>(&[5, 6])?;
    let args = host.vec_push_back(args, id1_obj.to_raw())?;
    // try call
    let val = host.call(id0_obj, sym, args)?;
    let exp: RawVal = 11i32.into();
    assert_eq!(val.get_payload(), exp.get_payload());
    Ok(())
}

#[test]
fn invoke_cross_contract_indirect_err() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let id0_obj = host.register_test_contract_wasm(INVOKE_CONTRACT)?;
    let id1_obj = host.register_test_contract_wasm(ADD_I32)?;
    let sym = Symbol::try_from_small_str("add_with").unwrap();
    let args = host.test_vec_obj::<i32>(&[i32::MAX, 1])?;
    let args = host.vec_push_back(args, id1_obj.into())?;

    // try call -- add will trap, and add_with will trap, but we will get a status
    let status = host.try_call(id0_obj, sym, args)?;
    let code = ScVmErrorCode::TrapUnreachable;
    let exp: Status = code.into();
    assert_eq!(status.get_payload(), exp.to_raw().get_payload());

    let events = host.get_events()?.0;
    assert_eq!(events.len(), 7);
    let last_event = events.last().unwrap();
    match &last_event.event {
        Event::Debug(de) => {
            assert_eq!(
                de.msg,
                Some("contract call invocation resulted in error {}".into())
            );
            assert_eq!(de.args.len(), 1);
            if let DebugArg::Val(rv) = de.args[0] {
                let status: Status = rv.try_into()?;
                assert_eq!(
                    status,
                    Status::from_type_and_code(
                        ScStatusType::VmError,
                        ScVmErrorCode::TrapUnreachable as u32,
                    )
                );
            } else {
                panic!("got: {:?}, want debug arg containing Val", de);
            }
        }
        _ => {
            panic!("got: {:?}, want debug event", last_event);
        }
    };

    // call
    let res = host.call(id0_obj, sym, args);
    assert!(HostError::result_matches_err_status(res, code));

    let events = host.get_events()?.0;
    assert_eq!(events.len(), 14);
    let last_event = events.last().unwrap();
    match &last_event.event {
        Event::Debug(de) => {
            assert_eq!(
                de.msg,
                Some("contract call invocation resulted in error {}".into())
            );
            assert_eq!(de.args.len(), 1);
            if let DebugArg::Val(rv) = de.args[0] {
                let status: Status = rv.try_into()?;
                assert_eq!(
                    status,
                    Status::from_type_and_code(
                        ScStatusType::VmError,
                        ScVmErrorCode::TrapUnreachable as u32,
                    )
                );
            } else {
                panic!("got: {:?}, want debug arg containing Val", de);
            }
        }
        _ => {
            panic!("got: {:?}, want debug event", last_event);
        }
    };

    Ok(())
}

#[test]
fn invoke_contract_with_reentry() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let id0_obj = host.register_test_contract_wasm(INVOKE_CONTRACT)?;
    // prepare arguments
    let sym = Symbol::try_from_small_str("add_with").unwrap();
    let args = host.test_vec_obj::<i32>(&[i32::MAX, 1])?;
    let args = host.vec_push_back(args, id0_obj.into())?; // trying to call its own `add` function

    // try call -- add will trap, and add_with will trap, but we will get a status
    let res = host.call(id0_obj, sym, args);
    let status = host.try_call(id0_obj, sym, args)?;
    let code = ScHostContextErrorCode::UnknownError;
    let exp: Status = code.into();
    assert!(HostError::result_matches_err_status(res, code));
    assert_eq!(status.get_payload(), exp.to_raw().get_payload());

    Ok(())
}
