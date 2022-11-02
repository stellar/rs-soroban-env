use crate::xdr::ScHostFnErrorCode;
use crate::{
    xdr::{ScHostObjErrorCode, ScObject, ScStatic, ScStatus, ScVal},
    CheckedEnv, Host, HostError, RawVal, RawValConvertible,
};
use soroban_env_common::EnvBase;

#[cfg(feature = "vm")]
use crate::{
    im_rc::OrdMap,
    storage::{AccessType, Footprint, Storage},
    xdr::{
        ContractDataEntry, Hash, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
        LedgerKeyContractData,
    },
    Object, Symbol,
};
#[cfg(feature = "vm")]
use soroban_test_wasms::LINEAR_MEMORY;

#[test]
fn bytes_suite_of_tests() -> Result<(), HostError> {
    let host = Host::default();
    // new and push
    let mut obj = host.bytes_new()?;
    for i in 0..32 {
        obj = host.bytes_push(obj, (i as u32).into())?;
    }
    if let ScObject::Bytes(b) = host.from_host_obj(obj)? {
        assert_eq!((0..32).collect::<Vec<u8>>().as_slice(), b.as_slice());
    } else {
        return Err(host.err_general("Type error"));
    }
    // pop and len
    for _ in 0..24 {
        obj = host.bytes_pop(obj)?;
    }
    assert_eq!(
        unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.bytes_len(obj)?) },
        8
    );
    assert_eq!(
        unsafe {
            <u32 as RawValConvertible>::unchecked_from_val(host.bytes_get(obj, 5_u32.into())?)
        },
        5
    );
    // put, del, get, front, back
    obj = host.bytes_put(obj, 5_u32.into(), 99_u32.into())?;
    assert_eq!(
        unsafe {
            <u32 as RawValConvertible>::unchecked_from_val(host.bytes_get(obj, 5_u32.into())?)
        },
        99
    );
    obj = host.bytes_del(obj, 5_u32.into())?; // [0,1,2,3,4,6,7]
    assert_eq!(
        unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.bytes_len(obj)?) },
        7
    );
    assert_eq!(
        unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.bytes_front(obj)?) },
        0
    );
    assert_eq!(
        unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.bytes_back(obj)?) },
        7
    );
    // insert, slice and append
    obj = host.bytes_insert(obj, 5_u32.into(), 5_u32.into())?; // [0,1,2,3,4,5,6,7]
    let obj0 = host.bytes_slice(obj, 0_u32.into(), 3_u32.into())?; // [0,1,2]
    if let ScObject::Bytes(b) = host.from_host_obj(obj0)? {
        assert_eq!((0..3).collect::<Vec<u8>>().as_slice(), b.as_slice());
    } else {
        return Err(host.err_general("Type error"));
    }
    let obj1 = host.bytes_slice(obj, 3_u32.into(), 8_u32.into())?; // [3,4,5,6,7]
    if let ScObject::Bytes(b) = host.from_host_obj(obj1)? {
        assert_eq!((3..8).collect::<Vec<u8>>().as_slice(), b.as_slice());
    } else {
        return Err(host.err_general("Type error"));
    }
    let obj_back = host.bytes_append(obj0, obj1)?;
    assert_eq!(host.obj_cmp(obj.into(), obj_back.into())?, 0);

    Ok(())
}

#[test]
fn bytes_put_out_of_bound() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.bytes_new()?;
    let res = host.bytes_put(obj, 0u32.into(), 1u32.into());
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn bytes_slice_start_greater_than_end() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.bytes_new_from_slice(&[1, 2, 3, 4])?;
    let res = host.bytes_slice(obj, 2_u32.into(), 1_u32.into());
    let code = ScHostFnErrorCode::InputArgsInvalid;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn bytes_slice_start_equal_len() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.bytes_new_from_slice(&[1, 2, 3, 4])?;
    let res = host.bytes_slice(obj, 4_u32.into(), 4_u32.into())?;
    assert_eq!(host.obj_cmp(res.into(), host.bytes_new()?.into())?, 0);
    Ok(())
}

#[test]
fn bytes_slice_start_greater_than_len() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.bytes_new_from_slice(&[1, 2, 3, 4])?;
    let res = host.bytes_slice(obj, 5_u32.into(), 10_u32.into());
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn bytes_xdr_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let roundtrip = |v: ScVal| -> Result<(), HostError> {
        let rv: RawVal = host.to_host_val(&v)?.into();
        let bo = host.serialize_to_bytes(rv.clone())?;
        let rv_back = host.deserialize_from_bytes(bo)?;
        assert_eq!(host.obj_cmp(rv, rv_back)?, 0);
        Ok(())
    };
    // u63
    roundtrip(ScVal::U63(5_i64))?;
    // u32
    roundtrip(ScVal::U32(23_u32))?;
    // i32
    roundtrip(ScVal::I32(-3_i32))?;
    // static
    roundtrip(ScVal::Static(ScStatic::True))?;
    // object
    {
        // vec
        let scval = ScVal::Object(Some(ScObject::Vec(host.test_scvec::<u32>(&[1, 2])?)));
        roundtrip(scval)?
        // TODO: add other types
    }
    // Symbol
    roundtrip(ScVal::Symbol(
        host.map_err("stellar".to_string().try_into())?,
    ))?;
    // bitset
    roundtrip(ScVal::Bitset(0xffffffff_u64))?;
    // status
    roundtrip(ScVal::Status(ScStatus::HostObjectError(
        ScHostObjErrorCode::UnknownError,
    )))?;

    Ok(())
}

#[cfg(feature = "vm")]
#[test]
fn linear_memory_operations() -> Result<(), HostError> {
    use crate::xdr::ScContractCode;
    use crate::{budget::Budget, host::metered_map::MeteredOrdMap};

    let contract_id: Hash = [0; 32].into();
    let key = ScVal::Static(ScStatic::LedgerKeyContractCode);
    let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
        contract_id: contract_id.clone(),
        key: key.clone(),
    });
    let val = ScVal::Object(Some(ScObject::ContractCode(ScContractCode::Wasm(
        LINEAR_MEMORY.try_into().unwrap(),
    ))));
    let le = LedgerEntry {
        last_modified_ledger_seq: 0,
        data: LedgerEntryData::ContractData(ContractDataEntry {
            contract_id,
            key,
            val,
        }),
        ext: LedgerEntryExt::V0,
    };
    let map = OrdMap::unit(Box::new(storage_key.clone()), Some(Box::new(le)));
    let mut footprint = Footprint::default();
    footprint.record_access(&storage_key, AccessType::ReadOnly)?;

    // initialize storage and host
    let storage = Storage::with_enforcing_footprint_and_map(
        footprint,
        MeteredOrdMap {
            map,
            budget: Budget::default(),
        },
    );
    let host = Host::with_storage_and_budget(storage, Budget::default());
    // create a dummy contract obj as the caller
    let id_obj = host.test_bin_obj(&[0; 32])?;
    // tests bytes_new_from_linear_memory
    {
        let args = host.test_vec_obj::<u32>(&[0xaabbccdd])?;
        let obj = host.call(
            id_obj.to_object(),
            Symbol::from_str("bin_word").into(),
            args.into(),
        )?;
        let obj_ref = host.test_bin_obj(&[0xaa, 0xbb, 0xcc, 0xdd])?;
        assert_eq!(host.obj_cmp(obj.into(), obj_ref.into())?, 0);
    }
    // tests bytes_copy_{to,from}_linear_memory
    {
        let obj0 = host.test_bin_obj(&[1, 2, 3, 4])?;
        let mut args = host.vec_new(RawVal::from_void())?;
        args = host.vec_push_back(args, obj0.to_raw())?;
        let obj: Object = host
            .call(
                id_obj.to_object(),
                Symbol::from_str("bin_inc").into(),
                args.into(),
            )?
            .try_into()?;
        let obj_ref = host.test_bin_obj(&[2, 3, 4, 5])?;
        assert_eq!(host.obj_cmp(obj.into(), obj_ref.into())?, 0);
    }

    Ok(())
}
