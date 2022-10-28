use soroban_env_common::{
    xdr::{ScHostContextErrorCode, ScVmErrorCode},
    RawVal,
};

use crate::{
    budget::Budget,
    events::{DebugArg, HostEvent},
    vm::Vm,
    xdr::{Hash, ScHostObjErrorCode, ScStatusType, ScVal, ScVec},
    CheckedEnv, Host, HostError, Status, Symbol, Tag,
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

#[test]
fn invoke_cross_contract() -> Result<(), HostError> {
    let dummy_id = [0; 32];
    let budget = Budget::default();
    let storage =
        Host::test_storage_with_contracts(vec![dummy_id.into()], vec![ADD_I32], budget.clone());
    let host = Host::with_storage_and_budget(storage, budget);
    let obj = host.test_bin_obj(&dummy_id)?;
    // prepare arguments
    let sym = Symbol::from_str("add");
    let args = host.test_vec_obj::<i32>(&[1, 2])?;
    let res = host.call(obj.to_object(), sym.into(), args.into())?;
    assert!(res.is::<i32>());
    assert!(res.get_tag() == Tag::I32);
    let i: i32 = res.try_into()?;
    assert_eq!(i, 3);
    Ok(())
}

#[test]
fn invoke_cross_contract_with_err() -> Result<(), HostError> {
    let dummy_id = [0; 32];
    let budget = Budget::default();
    let storage =
        Host::test_storage_with_contracts(vec![dummy_id.into()], vec![VEC], budget.clone());
    let host = Host::with_storage_and_budget(storage, budget);

    let obj = host.test_bin_obj(&dummy_id)?;
    // prepare arguments
    let sym = Symbol::from_str("vec_err");
    let args = host.test_vec_obj::<u32>(&[1])?;

    // try_call
    let sv = host.try_call(obj.to_object(), sym.into(), args.clone().into())?;
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    let exp_st: Status = code.into();
    assert_eq!(sv.get_payload(), exp_st.to_raw().get_payload());

    let events = host.get_events()?;
    assert_eq!(events.0.len(), 4);
    let last_event = events.0.last();
    match last_event {
        Some(HostEvent::Debug(de)) => {
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
    let res = host.call(obj.to_object(), sym.into(), args.into());
    assert!(HostError::result_matches_err_status(res, code));

    let events = host.get_events()?;
    assert_eq!(events.0.len(), 8);
    let last_event = events.0.last();
    match last_event {
        Some(HostEvent::Debug(de)) => {
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
    let dummy_id0 = [0; 32]; // the calling contract
    let dummy_id1 = [1; 32]; // the called contract
    let budget = Budget::default();
    let storage = Host::test_storage_with_contracts(
        vec![dummy_id0.into(), dummy_id1.into()],
        vec![INVOKE_CONTRACT, ADD_I32],
        budget.clone(),
    );
    let host = Host::with_storage_and_budget(storage, budget);
    // prepare arguments
    let id0_obj = host.test_bin_obj(&dummy_id0)?;
    let id1_obj = host.test_bin_obj(&dummy_id1)?;
    let sym = Symbol::from_str("add_with");
    let args = host.test_vec_obj::<i32>(&[5, 6])?;
    let args = host.vec_push_back(args.val, id1_obj.into())?;
    // try call
    let val = host.call(id0_obj.to_object(), sym.into(), args.clone().into())?;
    let exp: RawVal = 11i32.into();
    assert_eq!(val.get_payload(), exp.get_payload());
    Ok(())
}

#[test]
fn invoke_cross_contract_indirect_err() -> Result<(), HostError> {
    let dummy_id0 = [0; 32]; // the calling contract
    let dummy_id1 = [1; 32]; // the called contract
    let budget = Budget::default();
    let storage = Host::test_storage_with_contracts(
        vec![dummy_id0.into(), dummy_id1.into()],
        vec![INVOKE_CONTRACT, ADD_I32],
        budget.clone(),
    );
    let host = Host::with_storage_and_budget(storage, budget);
    // prepare arguments
    let id0_obj = host.test_bin_obj(&dummy_id0)?;
    let id1_obj = host.test_bin_obj(&dummy_id1)?;
    let sym = Symbol::from_str("add_with");
    let args = host.test_vec_obj::<i32>(&[i32::MAX, 1])?;
    let args = host.vec_push_back(args.val, id1_obj.into())?;

    // try call -- add will trap, and add_with will trap, but we will get a status
    let status = host.try_call(id0_obj.to_object(), sym.into(), args.clone().into())?;
    let code = ScVmErrorCode::TrapUnreachable;
    let exp: Status = code.into();
    assert_eq!(status.get_payload(), exp.to_raw().get_payload());

    let events = host.get_events()?;
    assert_eq!(events.0.len(), 5);
    let last_event = events.0.last();
    match last_event {
        Some(HostEvent::Debug(de)) => {
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
    let res = host.call(id0_obj.to_object(), sym.into(), args.clone().into());
    assert!(HostError::result_matches_err_status(res, code));

    let events = host.get_events()?;
    assert_eq!(events.0.len(), 10);
    let last_event = events.0.last();
    match last_event {
        Some(HostEvent::Debug(de)) => {
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
    let dummy_id0 = [0; 32]; // the calling contract
    let budget = Budget::default();
    let storage = Host::test_storage_with_contracts(
        vec![dummy_id0.into()],
        vec![INVOKE_CONTRACT],
        budget.clone(),
    );
    let host = Host::with_storage_and_budget(storage, budget);
    // prepare arguments
    let id0_obj = host.test_bin_obj(&dummy_id0)?;
    let sym = Symbol::from_str("add_with");
    let args = host.test_vec_obj::<i32>(&[i32::MAX, 1])?;
    let args = host.vec_push_back(args.val, id0_obj.clone().into())?; // trying to call its own `add` function

    // try call -- add will trap, and add_with will trap, but we will get a status
    let res = host.call(id0_obj.clone().to_object(), sym.into(), args.clone().into());
    let status = host.try_call(id0_obj.clone().to_object(), sym.into(), args.clone().into())?;
    let code = ScHostContextErrorCode::UnknownError;
    let exp: Status = code.into();
    assert!(HostError::result_matches_err_status(res, code));
    assert_eq!(status.get_payload(), exp.to_raw().get_payload());

    Ok(())
}
