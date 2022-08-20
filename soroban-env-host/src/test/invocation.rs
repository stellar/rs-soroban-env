use soroban_env_common::{xdr::ScVmErrorCode, RawVal};

use crate::{
    vm::Vm,
    xdr::{Hash, ScHostObjErrorCode, ScVal, ScVec},
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
    let storage = Host::test_storage_with_contracts(vec![dummy_id.into()], vec![ADD_I32]);
    let host = Host::with_storage(storage);
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
    let storage = Host::test_storage_with_contracts(vec![dummy_id.into()], vec![VEC]);
    let host = Host::with_storage(storage);

    let obj = host.test_bin_obj(&dummy_id)?;
    // prepare arguments
    let sym = Symbol::from_str("vec_err");
    let args = host.test_vec_obj::<u32>(&[1])?;

    // try_call
    let sv = host.try_call(obj.to_object(), sym.into(), args.clone().into())?;
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    let exp_st: Status = code.into();
    assert_eq!(sv.get_payload(), exp_st.to_raw().get_payload());

    // call
    let res = host.call(obj.to_object(), sym.into(), args.into());
    assert!(HostError::result_matches_err_status(res, code));

    Ok(())
}

#[test]
fn invoke_cross_contract_indirect() -> Result<(), HostError> {
    let dummy_id0 = [0; 32]; // the calling contract
    let dummy_id1 = [1; 32]; // the called contract
    let storage = Host::test_storage_with_contracts(
        vec![dummy_id0.into(), dummy_id1.into()],
        vec![INVOKE_CONTRACT, ADD_I32],
    );
    let host = Host::with_storage(storage);
    // prepare arguments
    let id0_obj = host.test_bin_obj(&dummy_id0)?;
    let id1_obj = host.test_bin_obj(&dummy_id1)?;
    let sym = Symbol::from_str("add_with");
    let args = host.test_vec_obj::<i32>(&[5, 6])?;
    let args = host.vec_push(args.val, id1_obj.into())?;
    // try call
    let val = host.call(id0_obj.to_object(), sym.into(), args.clone().into())?;
    let exp: RawVal = 11i32.into();
    assert_eq!(val.get_payload(), exp.get_payload());
    Ok(())
}

#[test]
fn invoke_cross_contract_indirect_err() -> Result<(), HostError> {
    let dummy_id0 = [0; 32]; // the calling contract
    let dummy_id1 = [1; 32]; // the called (failing) contract
    let storage = Host::test_storage_with_contracts(
        vec![dummy_id0.into(), dummy_id1.into()],
        vec![INVOKE_CONTRACT, ADD_I32],
    );
    let host = Host::with_storage(storage);
    // prepare arguments
    let id0_obj = host.test_bin_obj(&dummy_id0)?;
    let id1_obj = host.test_bin_obj(&dummy_id1)?;
    let sym = Symbol::from_str("add_with");
    let args = host.test_vec_obj::<i32>(&[i32::MAX, 1])?;
    let args = host.vec_push(args.val, id1_obj.into())?;
    // try call -- add will trap, and add_with will trap, but we will get a status
    let status = host.try_call(id0_obj.to_object(), sym.into(), args.clone().into())?;
    let exp: Status = ScVmErrorCode::TrapUnreachable.into();
    assert_eq!(status.get_payload(), exp.to_raw().get_payload());
    Ok(())
}
