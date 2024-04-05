use crate::{
    testutils::wasm,
    xdr::{ContractCostType, ScErrorCode, ScErrorType, ScVal},
    Compare, Env, Host, HostError, Object, Symbol, Tag, TryFromVal, U32Val, Val, VecObject,
};
use core::cmp::Ordering;
use more_asserts::assert_ge;
use soroban_test_wasms::LINEAR_MEMORY;
use std::{ops::Deref, time::Instant};

#[test]
fn vec_as_seen_by_host() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let val0 = host.test_vec_val(&[1u32])?;
    let val1 = host.test_vec_val(&[1u32])?;
    assert_eq!(val0.get_tag(), Tag::VecObject);
    assert_eq!(val1.get_tag(), Tag::VecObject);
    let obj0: Object = val0.try_into()?;
    let obj1: Object = val1.try_into()?;
    assert_eq!(obj0.get_handle(), 1);
    assert_eq!(obj1.get_handle(), 3);
    assert_eq!(obj0.as_val().get_tag(), Tag::VecObject);
    assert_eq!(obj1.as_val().get_tag(), Tag::VecObject);
    // Check that we got 2 distinct Vec objects
    assert_ne!(val0.get_payload(), val1.get_payload());
    // But also that they compare deep-equal.
    assert!((*host).compare(&val0, &val1).unwrap() == Ordering::Equal);
    Ok(())
}

#[test]
fn vec_new() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let v = host.vec_new()?;
    assert_eq!(
        host.vec_len(v)?.to_val().get_payload(),
        U32Val::from(0).to_val().get_payload()
    );
    Ok(())
}

#[test]
fn vec_front_and_back() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let front = u32::try_from(host.vec_front(obj)?)?;
    let back = u32::try_from(host.vec_back(obj)?)?;
    assert_eq!(front, 1);
    assert_eq!(back, 3);
    Ok(())
}

#[test]
fn empty_vec_front() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[])?;
    let res = host.vec_front(obj);
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn empty_vec_back() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[])?;
    let res = host.vec_back(obj);
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn vec_put_and_get() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let i: U32Val = 1_u32.into();
    let obj1 = host.vec_put(obj, i, 9_u32.into())?;
    let rv = host.vec_get(obj1, i)?;
    let v = u32::try_from(rv)?;
    assert_eq!(v, 9);
    Ok(())
}

#[test]
fn vec_push_pop_and_len() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[])?;
    let l: u32 = host.vec_len(obj)?.into();
    assert_eq!(l, 0);
    let obj1 = host.vec_push_back(obj, 1u32.into())?;
    let obj2 = host.vec_push_back(obj1, 2u32.into())?;
    let l: u32 = host.vec_len(obj2)?.into();
    assert_eq!(l, 2);
    let obj3 = host.vec_pop_back(obj2)?;
    let l: u32 = host.vec_len(obj3)?.into();
    assert_eq!(l, 1);
    let obj4 = host.vec_pop_back(obj3)?;
    let l: u32 = host.vec_len(obj4)?.into();
    assert_eq!(l, 0);
    Ok(())
}

#[test]
fn vec_pop_empty_vec() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[])?;
    let res = host.vec_pop_back(obj);
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn vec_push_pop_front() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[])?;
    let mut vec = host.vec_push_front(obj, 1u32.into())?;
    vec = host.vec_push_front(vec, 2u32.into())?;
    vec = host.vec_push_front(vec, 3u32.into())?;
    let mut vec_ref = host.test_vec_obj::<u32>(&[3, 2, 1])?;
    assert_eq!(host.obj_cmp(vec.into(), vec_ref.into())?, 0);
    vec = host.vec_pop_front(vec)?;
    vec_ref = host.test_vec_obj::<u32>(&[2, 1])?;
    assert_eq!(host.obj_cmp(vec.into(), vec_ref.into())?, 0);
    vec = host.vec_pop_front(vec)?;
    vec = host.vec_pop_front(vec)?;
    let res = host.vec_pop_front(vec);
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn vec_get_out_of_bound() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_get(obj, 3_u32.into());
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn vec_del_and_cmp() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let obj1 = host.vec_del(obj, 1u32.into())?;
    let obj_ref = host.test_vec_obj::<u32>(&[1, 3])?;
    assert_eq!(host.obj_cmp(obj1.into(), obj_ref.into())?, 0);
    Ok(())
}

#[test]
fn vec_del_out_of_bound() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_del(obj, 3_u32.into());
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn vec_slice_and_cmp() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let obj1 = host.vec_slice(obj, 1u32.into(), 3u32.into())?;
    let obj_ref = host.test_vec_obj::<u32>(&[2, 3])?;
    assert_eq!(host.obj_cmp(obj1.into(), obj_ref.into())?, 0);

    let obj2 = host.vec_slice(obj, 0u32.into(), 3u32.into())?;
    assert_ne!(obj2.as_val().get_payload(), obj.as_val().get_payload());
    assert_eq!(host.obj_cmp(obj2.into(), obj.into())?, 0);
    Ok(())
}

#[test]
fn vec_slice_start_equal_to_end() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let slice: ScVal = host
        .from_host_obj(host.vec_slice(obj, 1_u32.into(), 1_u32.into())?)?
        .into();
    let want = ScVal::Vec(Some(host.map_err(vec![].try_into())?));
    assert_eq!(slice, want);
    Ok(())
}

#[test]
fn vec_slice_start_greater_than_end() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_slice(obj, 2_u32.into(), 1_u32.into());
    let code = (ScErrorType::Object, ScErrorCode::InvalidInput);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn vec_slice_start_out_of_bound() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_slice(obj, 0_u32.into(), 4_u32.into());
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn vec_slice_end_out_of_bound() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_slice(obj, 0_u32.into(), 4_u32.into());
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn vec_insert_and_cmp() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[2])?;
    let obj1 = host.vec_insert(obj, 0u32.into(), 1u32.into())?;
    let obj_ref = host.test_vec_obj::<u32>(&[1, 2])?;
    assert_eq!(host.obj_cmp(obj1.into(), obj_ref.into())?, 0);

    let obj2 = host.vec_insert(obj1, 2u32.into(), 3u32.into())?;
    let obj_ref = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    assert_eq!(host.obj_cmp(obj2.into(), obj_ref.into())?, 0);

    let obj3 = host.vec_insert(obj2, 2u32.into(), 4u32.into())?;
    let obj_ref = host.test_vec_obj::<u32>(&[1, 2, 4, 3])?;
    assert_eq!(host.obj_cmp(obj3.into(), obj_ref.into())?, 0);
    Ok(())
}

#[test]
fn vec_insert_out_of_bound() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let res = host.vec_insert(obj, 4_u32.into(), 9u32.into());
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn vec_append() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj0 = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let obj1 = host.test_vec_obj::<u32>(&[4, 5, 6])?;
    let obj2 = host.vec_append(obj0, obj1)?;
    let obj_ref = host.test_vec_obj::<u32>(&[1, 2, 3, 4, 5, 6])?;
    assert_eq!(host.obj_cmp(obj2.into(), obj_ref.into())?, 0);
    Ok(())
}

#[test]
fn vec_append_empty() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj0 = host.test_vec_obj::<u32>(&[])?;
    let obj1 = host.vec_append(obj0, obj0)?;
    assert_ne!(obj0.as_val().get_payload(), obj1.as_val().get_payload());
    assert_eq!(host.obj_cmp(obj0.into(), obj1.into())?, 0);
    Ok(())
}

#[test]
fn vec_index_of() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj0 = host.test_vec_obj::<u32>(&[3, 4, 2, 2, 2, 5])?;
    let mut idx = host.vec_first_index_of(obj0, 2u32.into())?;
    assert_eq!(idx.get_payload(), Val::from(2u32).get_payload());
    idx = host.vec_last_index_of(obj0, 2u32.into())?;
    assert_eq!(idx.get_payload(), Val::from(4u32).get_payload());
    idx = host.vec_first_index_of(obj0, 1u32.into())?;
    assert_eq!(idx.get_payload(), Val::from_void().to_val().get_payload());
    idx = host.vec_last_index_of(obj0, 1u32.into())?;
    assert_eq!(idx.get_payload(), Val::from_void().to_val().get_payload());
    Ok(())
}

#[test]
fn vec_binary_search() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
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

#[test]
fn vec_build_bad_element_integrity() -> Result<(), HostError> {
    use crate::EnvBase;
    let host = observe_host!(Host::test_host());
    let obj = host.test_vec_obj::<u32>(&[1, 2, 3])?;
    let i = U32Val::from(1);

    let ok_val = obj.to_val();
    let payload = ok_val.get_payload();

    // The low 8 bits of an object-handle payload are the
    // tag indicating its type. We just add one to the
    // object type here, corrupting it.
    let bad_tag = Val::from_payload(payload + 1);

    // the high 32 bits of an object-handle payload are the
    // index number of the handle. We corrupt those here with
    // an object index far greater than any allocated.
    let bad_handle = Val::from_payload(payload | 0xff_u64 << 48);

    // Inserting ok object referejces into vectors should work.
    assert!(host.vec_put(obj, i, ok_val).is_ok());
    assert!(host.vec_push_front(obj, ok_val).is_ok());
    assert!(host.vec_push_back(obj, ok_val).is_ok());
    assert!(host.vec_new_from_slice(&[ok_val]).is_ok());

    // Inserting corrupt object references into vectors should fail.
    assert!(host.vec_put(obj, i, bad_tag).is_err());
    assert!(host.vec_push_front(obj, bad_tag).is_err());
    assert!(host.vec_push_back(obj, bad_tag).is_err());
    assert!(host.vec_new_from_slice(&[bad_tag]).is_err());

    assert!(host.vec_put(obj, i, bad_handle).is_err());
    assert!(host.vec_push_front(obj, bad_handle).is_err());
    assert!(host.vec_push_back(obj, bad_handle).is_err());
    assert!(host.vec_new_from_slice(&[bad_handle]).is_err());

    Ok(())
}

#[test]
fn linear_memory_operations() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let id_obj = host.register_test_contract_wasm(LINEAR_MEMORY);

    // Tests vec_unpack_to_linear_memory works when given correct input
    {
        let args = host.vec_new()?;
        let res = host.call(
            id_obj,
            Symbol::try_from_val(&*host, &"vec_mem_ok").unwrap(),
            args,
        );

        assert!(res.is_ok());
    }

    // Tests vec_unpack_to_linear_memory fails when given incorrect input
    {
        let args = host.vec_new()?;
        let res = host.call(
            id_obj,
            Symbol::try_from_val(&*host, &"vec_unpack_buf_too_short").unwrap(),
            args,
        );
        assert!(res.is_err());
        assert!(res.unwrap_err().error.is_code(ScErrorCode::UnexpectedSize));
    }
    {
        let args = host.vec_new()?;
        let res = host.call(
            id_obj,
            Symbol::try_from_val(&*host, &"vec_unpack_buf_too_long").unwrap(),
            args,
        );
        assert!(res.is_err());
        assert!(res.unwrap_err().error.is_code(ScErrorCode::UnexpectedSize));
    }
    Ok(())
}

#[test]
fn large_vec_exceeds_budget() {
    let host = Host::default();
    // Set a fixed budget higher than defaults, but still realistic.
    const MEMORY_LIMIT: u64 = 200 * 1024 * 1024;
    host.budget_ref()
        .reset_limits(200_000_000, MEMORY_LIMIT)
        .unwrap();
    let start = Instant::now();
    let mut v = host.vec_new().unwrap();
    let mut i = 0;
    loop {
        i += 1;
        let new_v_res = host.vec_push_back(v, U32Val::from(i).into());
        match new_v_res {
            Ok(new_v) => {
                v = new_v;
            }
            Err(e) => {
                assert!(e.error.is_type(ScErrorType::Budget));
                assert!(e.error.is_code(ScErrorCode::ExceededLimit));
                assert!(host.budget_ref().get_mem_bytes_consumed().unwrap() > MEMORY_LIMIT);
                break;
            }
        }
    }
    eprintln!(
        "total iterations: {}, real time: {}",
        i,
        (Instant::now() - start).as_secs_f64()
    );
}

#[test]
fn instantiate_oversized_vec_from_slice() -> Result<(), HostError> {
    use crate::EnvBase;
    let host = observe_host!(Host::test_host());

    let buf = vec![0; 7_000_000];
    let scv_bytes = ScVal::Bytes(buf.try_into().unwrap());
    let bytes_val = host.to_host_val(&scv_bytes)?;

    let vals = vec![bytes_val; 5];
    let vec = host.vec_new_from_slice(&vals.as_slice())?;
    let res = host.from_host_val(vec.to_val());
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

#[test]
fn instantiate_oversized_vec_from_linear_memory() -> Result<(), HostError> {
    let wasm_short =
        wasm::wasm_module_with_large_vector_from_linear_memory(100, U32Val::from(7).to_val());

    // sanity check, constructing a short vec is ok
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id_obj = host.register_test_contract_wasm(wasm_short.as_slice());
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("test")?,
        host.test_vec_obj::<u32>(&[])?,
    );
    assert!(res.is_ok());
    assert_eq!(
        host.vec_len(VecObject::try_from_val(host.deref(), &res?).unwrap())?
            .to_val()
            .get_payload(),
        U32Val::from(100).to_val().get_payload()
    );

    // constructing a big map will cause budget limit exceeded error
    let num_vals =
        if host.get_ledger_protocol_version()? < crate::vm::ModuleCache::MIN_LEDGER_VERSION {
            60_000
        } else {
            1_000_000
        };
    let wasm_long =
        wasm::wasm_module_with_large_vector_from_linear_memory(num_vals, U32Val::from(7).to_val());
    host.clear_module_cache()?;
    host.budget_ref().reset_unlimited()?;
    let contract_id_obj2 = host.register_test_contract_wasm(&wasm_long.as_slice());
    host.budget_ref().reset_default()?;
    let res = host.call(
        contract_id_obj2,
        Symbol::try_from_small_str("test")?,
        host.test_vec_obj::<u32>(&[])?,
    );
    // This currently won't pass VmInstantiation, in the future if VmInstantiation cost goes down, we need
    // to adjust the maximum length.
    // Here we check the mem inputs match expectation.
    assert_ge!(
        host.budget_ref()
            .get_tracker(ContractCostType::MemAlloc)?
            .inputs
            .unwrap(),
        480000
    );
    assert_ge!(
        host.budget_ref()
            .get_tracker(ContractCostType::MemCpy)?
            .inputs
            .unwrap(),
        480000
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));

    Ok(())
}
