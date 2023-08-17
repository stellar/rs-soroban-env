use crate::{
    xdr::{ScError, ScVal},
    Env, Host, HostError, Val,
};
use soroban_env_common::{
    xdr::{ScErrorCode, ScErrorType},
    Compare, EnvBase, Error,
};

use crate::Symbol;
use soroban_test_wasms::LINEAR_MEMORY;

#[test]
fn bytes_suite_of_tests() -> Result<(), HostError> {
    let host = Host::default();
    // new and push
    let mut obj = host.bytes_new()?;
    for i in 0..32 {
        obj = host.bytes_push(obj, (i as u32).into())?;
    }
    if let ScVal::Bytes(b) = host.from_host_val(obj.into())? {
        assert_eq!((0..32).collect::<Vec<u8>>().as_slice(), b.as_slice());
    } else {
        return Err(
            Error::from_type_and_code(ScErrorType::Object, ScErrorCode::UnexpectedType).into(),
        );
    }
    // pop and len
    for _ in 0..24 {
        obj = host.bytes_pop(obj)?;
    }
    assert_eq!(u32::from(host.bytes_len(obj)?), 8_u32);
    assert_eq!(u32::from(host.bytes_get(obj, 5_u32.into())?), 5_u32);
    // put, del, get, front, back
    obj = host.bytes_put(obj, 5_u32.into(), 99_u32.into())?;
    assert_eq!(u32::from(host.bytes_get(obj, 5_u32.into())?), 99_u32);
    obj = host.bytes_del(obj, 5_u32.into())?; // [0,1,2,3,4,6,7]
    assert_eq!(u32::from(host.bytes_len(obj)?), 7_u32);
    assert_eq!(u32::from(host.bytes_front(obj)?), 0_u32);
    assert_eq!(u32::from(host.bytes_back(obj)?), 7_u32);
    // insert, slice and append
    obj = host.bytes_insert(obj, 5_u32.into(), 5_u32.into())?; // [0,1,2,3,4,5,6,7]
    let obj0 = host.bytes_slice(obj, 0_u32.into(), 3_u32.into())?; // [0,1,2]
    if let ScVal::Bytes(b) = host.from_host_val(obj0.into())? {
        assert_eq!((0..3).collect::<Vec<u8>>().as_slice(), b.as_slice());
    } else {
        return Err(
            Error::from_type_and_code(ScErrorType::Object, ScErrorCode::InternalError).into(),
        );
    }
    let obj1 = host.bytes_slice(obj, 3_u32.into(), 8_u32.into())?; // [3,4,5,6,7]
    if let ScVal::Bytes(b) = host.from_host_val(obj1.into())? {
        assert_eq!((3..8).collect::<Vec<u8>>().as_slice(), b.as_slice());
    } else {
        return Err(
            Error::from_type_and_code(ScErrorType::Object, ScErrorCode::InternalError).into(),
        );
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
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn bytes_slice_start_greater_than_end() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.bytes_new_from_slice(&[1, 2, 3, 4])?;
    let res = host.bytes_slice(obj, 2_u32.into(), 1_u32.into());
    let code = (ScErrorType::Object, ScErrorCode::InvalidInput);
    assert!(HostError::result_matches_err(res, code));
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
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn bytes_xdr_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let roundtrip = |v: ScVal| -> Result<(), HostError> {
        let rv: Val = host.to_host_val(&v)?;
        let bo = host.serialize_to_bytes(rv)?;
        let rv_back = host.deserialize_from_bytes(bo)?;
        assert_eq!(host.compare(&rv, &rv_back)?, core::cmp::Ordering::Equal);
        Ok(())
    };
    // u64
    roundtrip(ScVal::U64(5_u64))?;
    // u32
    roundtrip(ScVal::U32(23_u32))?;
    // i32
    roundtrip(ScVal::I32(-3_i32))?;
    // static
    roundtrip(ScVal::Bool(true))?;
    // object
    {
        // vec
        let scval = ScVal::Vec(Some(host.test_scvec::<u32>(&[1, 2])?));
        roundtrip(scval)?
        // TODO: add other types
    }
    // Symbol
    roundtrip(ScVal::Symbol(crate::xdr::ScSymbol(
        host.map_err("stellar".to_string().try_into())?,
    )))?;
    // error
    roundtrip(ScVal::Error(ScError::Context(ScErrorCode::InternalError)))?;
    Ok(())
}

#[test]
fn linear_memory_operations() -> Result<(), HostError> {
    use soroban_env_common::BytesObject;

    let host = Host::test_host_with_recording_footprint();
    let id_obj = host.register_test_contract_wasm(LINEAR_MEMORY);
    // tests bytes_new_from_linear_memory
    {
        let args = host.test_vec_obj::<u32>(&[0xaabbccdd])?;
        let obj: BytesObject = host
            .call(
                id_obj,
                Symbol::try_from_small_str("bin_word").unwrap(),
                args,
            )?
            .try_into()?;
        let obj_ref: BytesObject = host.test_bin_obj(&[0xaa, 0xbb, 0xcc, 0xdd])?;
        assert_eq!(
            host.compare(&obj.to_val(), &obj_ref.to_val())?,
            core::cmp::Ordering::Equal
        );
    }
    // tests bytes_copy_{to,from}_linear_memory
    {
        let obj0 = host.test_bin_obj(&[1, 2, 3, 4])?;
        let mut args = host.vec_new()?;
        args = host.vec_push_back(args, obj0.to_val())?;
        let obj: BytesObject = host
            .call(id_obj, Symbol::try_from_small_str("bin_inc").unwrap(), args)?
            .try_into()?;
        let obj_ref = host.test_bin_obj(&[2, 3, 4, 5])?;
        assert_eq!(
            host.compare(&obj.to_val(), &obj_ref.to_val())?,
            core::cmp::Ordering::Equal
        );
    }

    Ok(())
}
