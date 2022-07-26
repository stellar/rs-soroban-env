use super::wasm_examples::{ADD_I32, CALL, VEC};
use crate::{
    im_rc::OrdMap,
    storage::{AccessType, Footprint, Storage},
    vm::Vm,
    xdr::{
        ContractDataEntry, Hash, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
        LedgerKeyContractData, ScHostObjErrorCode, ScObject, ScStatic, ScVal, ScVec,
    },
    CheckedEnv, Host, HostError, Status, Symbol, Tag,
};

#[test]
fn invoke_single_contract_function() -> Result<(), HostError> {
    use stellar_contract_env_common::xdr::ScVmErrorCode;

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
    let contract_id: Hash = [0; 32].into();
    let key = ScVal::Static(ScStatic::LedgerKeyContractCodeWasm);
    let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
        contract_id: contract_id.clone(),
        key: key.clone(),
    });
    // We unwrap here rather than host.map_err because the host doesn't exist yet.
    let scob = ScObject::Binary(ADD_I32.try_into().unwrap());
    let val = ScVal::Object(Some(scob));
    let le = LedgerEntry {
        last_modified_ledger_seq: 0,
        data: LedgerEntryData::ContractData(ContractDataEntry {
            contract_id,
            key,
            val,
        }),
        ext: LedgerEntryExt::V0,
    };
    let map = OrdMap::unit(storage_key.clone(), Some(le));
    let mut footprint = Footprint::default();
    footprint.record_access(&storage_key, AccessType::ReadOnly);

    // initialize storage and host
    let storage = Storage::with_enforcing_footprint_and_map(footprint, map);
    let host = Host::with_storage(storage);
    // create a dummy contract obj as the caller
    let obj = host.test_bin_obj(&[0; 32])?;
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
    let contract_id: Hash = [0; 32].into();
    let key = ScVal::Static(ScStatic::LedgerKeyContractCodeWasm);
    let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
        contract_id: contract_id.clone(),
        key: key.clone(),
    });
    // We unwrap here rather than host.map_err because the host doesn't exist yet.
    let scob = ScObject::Binary(VEC.try_into().unwrap());
    let val = ScVal::Object(Some(scob));
    let le = LedgerEntry {
        last_modified_ledger_seq: 0,
        data: LedgerEntryData::ContractData(ContractDataEntry {
            contract_id,
            key,
            val,
        }),
        ext: LedgerEntryExt::V0,
    };
    let map = OrdMap::unit(storage_key.clone(), Some(le));

    let mut footprint = Footprint::default();
    footprint.record_access(&storage_key, AccessType::ReadOnly);

    // initialize storage and host
    let storage = Storage::with_enforcing_footprint_and_map(footprint, map);
    let host = Host::with_storage(storage);
    // create a dummy contract obj as the caller
    let obj = host.test_bin_obj(&[0; 32])?;
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
fn invoke_cross_contract_lvl2_nested_with_err() -> Result<(), HostError> {
    // 1st level, the calling contract
    let id0: Hash = [0; 32].into();
    let key = ScVal::Static(ScStatic::LedgerKeyContractCodeWasm);
    let storage_key0 = LedgerKey::ContractData(LedgerKeyContractData {
        contract_id: id0.clone(),
        key: key.clone(),
    });
    let scob0 = ScObject::Binary(CALL.try_into().unwrap());
    let val0 = ScVal::Object(Some(scob0));
    let le0 = LedgerEntry {
        last_modified_ledger_seq: 0,
        data: LedgerEntryData::ContractData(ContractDataEntry {
            contract_id: id0,
            key: key.clone(),
            val: val0,
        }),
        ext: LedgerEntryExt::V0,
    };

    // 2nd level, the guest contract
    let id1: Hash = [1; 32].into();
    let storage_key1 = LedgerKey::ContractData(LedgerKeyContractData {
        contract_id: id1.clone(),
        key: key.clone(),
    });
    let scob1 = ScObject::Binary(VEC.try_into().unwrap());
    let val1 = ScVal::Object(Some(scob1));
    let le1 = LedgerEntry {
        last_modified_ledger_seq: 0,
        data: LedgerEntryData::ContractData(ContractDataEntry {
            contract_id: id1,
            key,
            val: val1,
        }),
        ext: LedgerEntryExt::V0,
    };

    // create storage map and footprint
    let mut map = OrdMap::unit(storage_key0.clone(), Some(le0));
    map.insert(storage_key1.clone(), Some(le1));
    let mut footprint = Footprint::default();
    footprint.record_access(&storage_key0, AccessType::ReadOnly);
    footprint.record_access(&storage_key1, AccessType::ReadOnly);

    // initialize storage and host
    let storage = Storage::with_enforcing_footprint_and_map(footprint, map);
    let host = Host::with_storage(storage);
    // prepare arguments
    let obj = host.test_bin_obj(&[0; 32])?;
    let sym = Symbol::from_str("delegate");
    let args = host.test_vec_obj::<u32>(&[1])?;
    // try call
    let sv = host.try_call(obj.to_object(), sym.into(), args.clone().into())?;
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    let exp_st: Status = code.into();
    assert_eq!(sv.get_payload(), exp_st.as_raw().get_payload());
    // call
    let res = host.call(obj.to_object(), sym.into(), args.into());
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}
