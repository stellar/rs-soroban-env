use crate::{
    budget::AsBudget,
    test::wasm_util,
    xdr::{ContractCostType, ScBytes, ScError, ScErrorCode, ScErrorType, ScVal, ScVec, WriteXdr},
    BytesObject, Compare, Env, EnvBase, Error, Host, HostError, Symbol, TryFromVal, U32Val, Val,
    DEFAULT_XDR_RW_LIMITS,
};
use more_asserts::assert_ge;
use soroban_test_wasms::LINEAR_MEMORY;

#[test]
fn bytes_suite_of_tests() -> Result<(), HostError> {
    let host = observe_host!(Host::default());
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
    let host = observe_host!(Host::default());
    let obj = host.bytes_new()?;
    let res = host.bytes_put(obj, 0u32.into(), 1u32.into());
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn bytes_slice_start_greater_than_end() -> Result<(), HostError> {
    let host = observe_host!(Host::default());
    let obj = host.bytes_new_from_slice(&[1, 2, 3, 4])?;
    let res = host.bytes_slice(obj, 2_u32.into(), 1_u32.into());
    let code = (ScErrorType::Object, ScErrorCode::InvalidInput);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn bytes_slice_start_equal_len() -> Result<(), HostError> {
    let host = observe_host!(Host::default());
    let obj = host.bytes_new_from_slice(&[1, 2, 3, 4])?;
    let res = host.bytes_slice(obj, 4_u32.into(), 4_u32.into())?;
    assert_eq!(host.obj_cmp(res.into(), host.bytes_new()?.into())?, 0);
    Ok(())
}

#[test]
fn bytes_slice_start_greater_than_len() -> Result<(), HostError> {
    let host = observe_host!(Host::default());
    let obj = host.bytes_new_from_slice(&[1, 2, 3, 4])?;
    let res = host.bytes_slice(obj, 5_u32.into(), 10_u32.into());
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn bytes_xdr_roundtrip() -> Result<(), HostError> {
    let host = observe_host!(Host::default());
    let roundtrip = |v: ScVal| -> Result<(), HostError> {
        let rv: Val = host.to_host_val(&v)?;
        let bo = host.serialize_to_bytes(rv)?;
        let rv_back = host.deserialize_from_bytes(bo)?;
        assert_eq!((*host).compare(&rv, &rv_back)?, core::cmp::Ordering::Equal);
        Ok(())
    };
    let deser_fails_bytes = |bytes: Vec<u8>| -> Result<(), HostError> {
        let bo = host.add_host_object(ScBytes(bytes.try_into()?))?;
        let res = host.deserialize_from_bytes(bo);
        assert!(res.is_err());
        assert!(res.err().unwrap().error.is_code(ScErrorCode::InvalidInput));
        Ok(())
    };
    let deser_fails_scv = |v: ScVal| -> Result<(), HostError> {
        let bytes: Vec<u8> = v.to_xdr(DEFAULT_XDR_RW_LIMITS)?;
        let bo = host.add_host_object(ScBytes(bytes.try_into()?))?;
        let res = host.deserialize_from_bytes(bo);
        assert!(res.is_err());
        assert!(res
            .err()
            .unwrap()
            .error
            .is_code(ScErrorCode::UnexpectedType));
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
    // garbage bytes fail
    deser_fails_bytes(vec![1, 2, 3, 4, 5])?;
    // non-representable fails
    deser_fails_scv(ScVal::LedgerKeyContractInstance)?;
    // non-representable fails
    deser_fails_scv(ScVal::Vec(Some(ScVec(
        vec![ScVal::LedgerKeyContractInstance].try_into()?,
    ))))?;
    Ok(())
}

#[test]
fn linear_memory_operations() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
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
            (*host).compare(&obj.to_val(), &obj_ref.to_val())?,
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
            (*host).compare(&obj.to_val(), &obj_ref.to_val())?,
            core::cmp::Ordering::Equal
        );
    }

    Ok(())
}

#[test]
fn test_bytes_out_of_cpu_budget() -> Result<(), HostError> {
    let host = observe_host!(Host::default());
    host.as_budget().reset_unlimited_cpu()?;
    let mut b1 = host.bytes_new_from_slice(&[2; 1])?;
    loop {
        let res = host.bytes_append(b1, b1.clone());
        if res.is_err() {
            assert!(HostError::result_matches_err(
                res,
                (ScErrorType::Budget, ScErrorCode::ExceededLimit)
            ));
            break;
        }
        b1 = res?;
    }
    assert!(host.budget_ref().mem_limit_exceeded()?);
    Ok(())
}

#[test]
fn test_bytes_out_of_mem_budget() -> Result<(), HostError> {
    let host = observe_host!(Host::default());
    host.as_budget().reset_unlimited_mem()?;
    let mut b1 = host.bytes_new_from_slice(&[2; 1])?;
    loop {
        let res = host.bytes_append(b1, b1.clone());
        if res.is_err() {
            assert!(HostError::result_matches_err(
                res,
                (ScErrorType::Budget, ScErrorCode::ExceededLimit)
            ));
            break;
        }
        b1 = res?;
    }
    assert!(host.budget_ref().cpu_limit_exceeded()?);
    Ok(())
}

#[test]
fn instantiate_oversized_bytes_from_slice() -> Result<(), HostError> {
    use crate::EnvBase;
    let host = observe_host!(Host::default());

    let buf = vec![0; 42_000_000];
    let res = host.bytes_new_from_slice(&buf);
    assert_eq!(
        host.budget_ref()
            .get_tracker(ContractCostType::MemAlloc)?
            .1
            .unwrap(),
        42_000_000
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

#[test]
fn instantiate_oversized_bytes_from_linear_memory() -> Result<(), HostError> {
    let wasm_short = wasm_util::wasm_module_with_large_bytes_from_linear_memory(100, 7);

    // sanity check, constructing a short vec is ok
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(wasm_short.as_slice());
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("test")?,
        host.test_vec_obj::<u32>(&[])?,
    );
    assert!(res.is_ok());
    assert_eq!(
        host.bytes_len(BytesObject::try_from_val(&host, &res?).unwrap())?
            .to_val()
            .get_payload(),
        U32Val::from(100).to_val().get_payload()
    );

    // constructing a big map will cause budget limit exceeded error
    let wasm_long = wasm_util::wasm_module_with_large_bytes_from_linear_memory(240000, 7);
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
            .1
            .unwrap(),
        240000
    );
    assert_ge!(
        host.budget_ref()
            .get_tracker(ContractCostType::MemCpy)?
            .1
            .unwrap(),
        240000
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));

    Ok(())
}
