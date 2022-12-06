use core::cmp::Ordering;

use soroban_env_common::Compare;

use crate::{
    xdr::{ScHostFnErrorCode, ScHostObjErrorCode, ScObject, ScObjectType},
    CheckedEnv, Host, HostError, Object, RawVal, RawValConvertible,
};

#[test]
fn vec_as_seen_by_host() -> Result<(), HostError> {
    let host = Host::default();
    let val0 = host.test_vec_val(&[1u32])?;
    let val1 = host.test_vec_val(&[1u32])?;
    assert!(val0.is::<Object>());
    assert!(val1.is::<Object>());
    let obj0: Object = val0.try_into()?;
    let obj1: Object = val1.try_into()?;
    assert_eq!(obj0.get_handle(), 0);
    assert_eq!(obj1.get_handle(), 1);
    assert!(obj0.is_obj_type(ScObjectType::Vec));
    assert!(obj1.is_obj_type(ScObjectType::Vec));
    // Check that we got 2 distinct Vec objects
    assert_ne!(val0.get_payload(), val1.get_payload());
    // But also that they compare deep-equal.
    assert!(host.compare(&val0, &val1).unwrap() == Ordering::Equal);
    Ok(())
}

#[test]
fn vec_new_with_capacity() -> Result<(), HostError> {
    let host = Host::default();
    host.vec_new(RawVal::from_void())?;
    host.vec_new(5_u32.into())?;
    let code = ScHostFnErrorCode::InputArgsWrongType;
    let res = host.vec_new(5_i32.into());
    assert!(HostError::result_matches_err_status(res, code));
    let res = host.vec_new(RawVal::from_bool(true));
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_front_and_back() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let front = unsafe { <i32 as RawValConvertible>::unchecked_from_val(host.vec_front(obj)?) };
    let back = unsafe { <i32 as RawValConvertible>::unchecked_from_val(host.vec_back(obj)?) };
    assert_eq!(front, 1);
    assert_eq!(back, 3);
    Ok(())
}

#[test]
fn empty_vec_front() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[])?;
    let res = host.vec_front(obj);
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn empty_vec_back() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[])?;
    let res = host.vec_back(obj);
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_put_and_get() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let i: RawVal = 1_u32.into();
    let obj1 = host.vec_put(obj, i, 9_u32.into())?;
    let rv = host.vec_get(obj1, i)?;
    let v = unsafe { <u32 as RawValConvertible>::unchecked_from_val(rv) };
    assert_eq!(v, 9);
    Ok(())
}

#[test]
fn vec_push_pop_and_len() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[])?;
    let l = unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.vec_len(obj)?) };
    assert_eq!(l, 0);
    let obj1 = host.vec_push_back(obj, 1u32.into())?;
    let obj2 = host.vec_push_back(obj1, 2u32.into())?;
    let l = unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.vec_len(obj2)?) };
    assert_eq!(l, 2);
    let obj3 = host.vec_pop_back(obj2)?;
    let l = unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.vec_len(obj3)?) };
    assert_eq!(l, 1);
    let obj4 = host.vec_pop_back(obj3)?;
    let l = unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.vec_len(obj4)?) };
    assert_eq!(l, 0);
    Ok(())
}

#[test]
fn vec_pop_empty_vec() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[])?;
    let res = host.vec_pop_back(obj);
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_push_pop_front() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[])?;
    let mut vec = host.vec_push_front(obj, 1u32.into())?;
    vec = host.vec_push_front(vec, 2u32.into())?;
    vec = host.vec_push_front(vec, 3u32.into())?;
    let mut vec_ref = host.test_vec_obj::<u32>(&[3, 2, 1])?;
    assert_eq!(host.obj_cmp(vec.to_raw(), vec_ref.to_raw())?, 0);
    vec = host.vec_pop_front(vec)?;
    vec_ref = host.test_vec_obj::<u32>(&[2, 1])?;
    assert_eq!(host.obj_cmp(vec.to_raw(), vec_ref.to_raw())?, 0);
    vec = host.vec_pop_front(vec)?;
    vec = host.vec_pop_front(vec)?;
    let res = host.vec_pop_front(vec);
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_get_out_of_bound() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_get(obj, 3_u32.into());
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_get_wrong_index_type() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[])?;
    let res = host.vec_get(obj, (-1_i32).into());
    let code = ScHostFnErrorCode::InputArgsWrongType;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_del_and_cmp() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let obj1 = host.vec_del(obj, 1u32.into())?;
    let obj_ref = host.test_vec_obj::<u32>(&[1, 3])?;
    assert_eq!(host.obj_cmp(obj1.into(), obj_ref.into())?, 0);
    Ok(())
}

#[test]
fn vec_del_out_of_bound() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_del(obj, 3_u32.into());
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_del_wrong_index_type() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[])?;
    let res = host.vec_del(obj, (-1_i32).into());
    let code = ScHostFnErrorCode::InputArgsWrongType;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_slice_and_cmp() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let obj1 = host.vec_slice(obj, 1u32.into(), 3u32.into())?;
    let obj_ref = host.test_vec_obj::<u32>(&[2, 3])?;
    assert_eq!(host.obj_cmp(obj1.into(), obj_ref.into())?, 0);

    let obj2 = host.vec_slice(obj, 0u32.into(), 3u32.into())?;
    assert_ne!(obj2.as_raw().get_payload(), obj.as_raw().get_payload());
    assert_eq!(host.obj_cmp(obj2.into(), obj.into())?, 0);
    Ok(())
}

#[test]
fn vec_slice_start_equal_to_end() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let slice = host.from_host_obj(host.vec_slice(obj, 1_u32.into(), 1_u32.into())?)?;
    let want = ScObject::Vec(host.map_err(vec![].try_into())?);
    assert_eq!(slice, want);
    Ok(())
}

#[test]
fn vec_slice_start_greater_than_end() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_slice(obj, 2_u32.into(), 1_u32.into());
    let code = ScHostFnErrorCode::InputArgsInvalid;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_slice_start_out_of_bound() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_slice(obj, 0_u32.into(), 4_u32.into());
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_slice_end_out_of_bound() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_slice(obj, 0_u32.into(), 4_u32.into());
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_take_wrong_index_type() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[])?;
    let res = host.vec_slice(obj, (-1_i32).into(), 1_u32.into());
    let code = ScHostFnErrorCode::InputArgsWrongType;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_take_wrong_len_type() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[])?;
    let res = host.vec_slice(obj, 1_u32.into(), (-1_i32).into());
    let code = ScHostFnErrorCode::InputArgsWrongType;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_insert_and_cmp() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[2])?;
    let obj1 = host.vec_insert(obj, 0u32.into(), 1u32.into())?;
    let obj_ref = host.test_vec_obj::<u32>(&[1, 2])?;
    assert_eq!(host.obj_cmp(obj1.into(), obj_ref.into())?, 0);

    let obj2 = host.vec_insert(obj1, 2u32.into(), 3u32.into())?;
    let obj_ref = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    assert_eq!(host.obj_cmp(obj2.into(), obj_ref.into())?, 0);
    Ok(())
}

#[test]
fn vec_insert_out_of_bound() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_insert(obj, 4_u32.into(), 9u32.into());
    let code = ScHostObjErrorCode::VecIndexOutOfBound;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_insert_wrong_index_type() -> Result<(), HostError> {
    let host = Host::default();
    let obj = host.test_vec_obj::<u32>(&[])?;
    let res = host.vec_insert(obj, (-1_i32).into(), 9u32.into());
    let code = ScHostFnErrorCode::InputArgsWrongType;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn vec_append() -> Result<(), HostError> {
    let host = Host::default();
    let obj0 = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let obj1 = host.test_vec_obj::<u32>(&[4, 5, 6])?;
    let obj2 = host.vec_append(obj0, obj1)?;
    let obj_ref = host.test_vec_obj::<u32>(&[1, 2, 3, 4, 5, 6])?;
    assert_eq!(host.obj_cmp(obj2.into(), obj_ref.into())?, 0);
    Ok(())
}

#[test]
fn vec_append_empty() -> Result<(), HostError> {
    let host = Host::default();
    let obj0 = host.test_vec_obj::<u32>(&[])?;
    let obj1 = host.vec_append(obj0, obj0)?;
    assert_ne!(obj0.as_raw().get_payload(), obj1.as_raw().get_payload());
    assert_eq!(host.obj_cmp(obj0.into(), obj1.into())?, 0);
    Ok(())
}

#[test]
fn vec_index_of() -> Result<(), HostError> {
    let host = Host::default();
    let obj0 = host.test_vec_obj::<u32>(&[3, 4, 2, 2, 2, 5])?;
    let mut idx = host.vec_first_index_of(obj0, 2u32.into())?;
    assert_eq!(idx.get_payload(), RawVal::from(2u32).get_payload());
    idx = host.vec_last_index_of(obj0, 2u32.into())?;
    assert_eq!(idx.get_payload(), RawVal::from(4u32).get_payload());
    idx = host.vec_first_index_of(obj0, 1u32.into())?;
    assert_eq!(idx.get_payload(), RawVal::from_void().get_payload());
    idx = host.vec_last_index_of(obj0, 1u32.into())?;
    assert_eq!(idx.get_payload(), RawVal::from_void().get_payload());
    Ok(())
}

#[test]
fn vec_binary_search() -> Result<(), HostError> {
    let host = Host::default();
    let obj0 = host.test_vec_obj::<u32>(&[1, 2, 4, 5, 7, 9])?;
    let mut res = host.vec_binary_search(obj0, 7u32.into())?;
    let exp: u64 = 4 | (1 << 32);
    assert_eq!(res, exp);
    res = host.vec_binary_search(obj0, 4u32.into())?;
    let exp: u64 = 2 | (1 << 32);
    assert_eq!(res, exp);
    res = host.vec_binary_search(obj0, 3u32.into())?;
    assert_eq!(u64::from(2u32), res);
    res = host.vec_binary_search(obj0, 6u32.into())?;
    assert_eq!(u64::from(4u32), res);
    Ok(())
}
