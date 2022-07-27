use crate::{
    xdr::{ScHostObjErrorCode, ScObject, ScStatic, ScStatus, ScVal},
    CheckedEnv, Host, HostError, RawVal, RawValConvertible,
};

#[cfg(feature = "vm")]
use super::wasm_examples::LINEAR_MEMORY;
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

#[test]
fn binary_suite_of_tests() -> Result<(), HostError> {
    let host = Host::default();
    // new and push
    let mut obj = host.binary_new()?;
    for i in 0..32 {
        obj = host.binary_push(obj, (i as u32).into())?;
    }
    if let ScObject::Binary(b) = host.from_host_obj(obj)? {
        assert_eq!((0..32).collect::<Vec<u8>>().as_slice(), b.as_slice());
    } else {
        return Err(host.err_general("Type error"));
    }
    // pop and len
    for _ in 0..24 {
        obj = host.binary_pop(obj)?;
    }
    assert_eq!(
        unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.binary_len(obj)?) },
        8
    );
    assert_eq!(
        unsafe {
            <u32 as RawValConvertible>::unchecked_from_val(host.binary_get(obj, 5_u32.into())?)
        },
        5
    );
    // put, del, get, front, back
    obj = host.binary_put(obj, 5_u32.into(), 99_u32.into())?;
    assert_eq!(
        unsafe {
            <u32 as RawValConvertible>::unchecked_from_val(host.binary_get(obj, 5_u32.into())?)
        },
        99
    );
    obj = host.binary_del(obj, 5_u32.into())?; // [0,1,2,3,4,6,7]
    assert_eq!(
        unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.binary_len(obj)?) },
        7
    );
    assert_eq!(
        unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.binary_front(obj)?) },
        0
    );
    assert_eq!(
        unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.binary_back(obj)?) },
        7
    );
    // insert, slice and append
    obj = host.binary_insert(obj, 5_u32.into(), 5_u32.into())?; // [0,1,2,3,4,5,6,7]
    let obj0 = host.binary_slice(obj, 0_u32.into(), 3_u32.into())?; // [0,1,2]
    if let ScObject::Binary(b) = host.from_host_obj(obj0)? {
        assert_eq!((0..3).collect::<Vec<u8>>().as_slice(), b.as_slice());
    } else {
        return Err(host.err_general("Type error"));
    }
    let obj1 = host.binary_slice(obj, 3_u32.into(), 8_u32.into())?; // [3,4,5,6,7]
    if let ScObject::Binary(b) = host.from_host_obj(obj1)? {
        assert_eq!((3..8).collect::<Vec<u8>>().as_slice(), b.as_slice());
    } else {
        return Err(host.err_general("Type error"));
    }
    let obj_back = host.binary_append(obj0, obj1)?;
    assert_eq!(host.obj_cmp(obj.into(), obj_back.into())?, 0);

    Ok(())
}

#[test]
fn binary_put_out_of_bound() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.binary_new()?;
    let res = host.binary_put(obj, 0u32.into(), 1u32.into());
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn binary_xdr_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let roundtrip = |v: ScVal| -> Result<(), HostError> {
        let rv: RawVal = host.to_host_val(&v)?.into();
        let bo = host.serialize_to_binary(rv.clone())?;
        let rv_back = host.deserialize_from_binary(bo)?;
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
fn invoke_memcpy() -> Result<(), HostError> {
    let contract_id: Hash = [0; 32].into();
    let key = ScVal::Static(ScStatic::LedgerKeyContractCodeWasm);
    let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
        contract_id: contract_id.clone(),
        key: key.clone(),
    });
    let scob = ScObject::Binary(LINEAR_MEMORY.try_into().unwrap());
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
    let id_obj = host.test_bin_obj(&[0; 32])?;
    // binary_new_from_linear_memory
    {
        let args = host.test_vec_obj::<u32>(&[4])?;
        let obj = host.call(
            id_obj.to_object(),
            Symbol::from_str("bin_new").into(),
            args.into(),
        )?;
        let obj_ref = host.test_bin_obj(&[0, 1, 2, 3])?;
        assert_eq!(host.obj_cmp(obj.into(), obj_ref.into())?, 0);
    }
    // binary_copy_from_linear_memory
    {
        let obj0 = host.binary_new()?;
        let mut args = host.vec_new(RawVal::from_void())?;
        args = host.vec_push(args, obj0.to_raw())?;
        args = host.vec_push(args, 0_u32.into())?;
        args = host.vec_push(args, 1_u32.into())?;
        args = host.vec_push(args, 3_u32.into())?;
        let obj: Object = host
            .call(
                id_obj.to_object(),
                Symbol::from_str("from_guest").into(),
                args.into(),
            )?
            .try_into()?;
        let obj_ref = host.test_bin_obj(&[1, 2, 3])?;
        assert_eq!(host.obj_cmp(obj.into(), obj_ref.into())?, 0);
    }
    // binary_copy_to_linear_memory
    {
        let obj0 = host.test_bin_obj(&[0, 1, 2, 3])?;
        let mut args = host.vec_new(RawVal::from_void())?;
        args = host.vec_push(args, obj0.to_raw())?;
        args = host.vec_push(args, 0_u32.into())?;
        args = host.vec_push(args, 0_u32.into())?;
        args = host.vec_push(args, 4_u32.into())?;
        host.call(
            id_obj.to_object(),
            Symbol::from_str("to_guest").into(),
            args.into(),
        )?;
    }

    Ok(())
}
