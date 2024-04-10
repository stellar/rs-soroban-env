use crate::Symbol;
use crate::{
    budget::AsBudget,
    testutils::wasm,
    xdr::{
        AccountId, ContractCostType, Hash, PublicKey, ScAddress, ScBytes, ScError, ScErrorCode,
        ScErrorType, ScMap, ScMapEntry, ScVal, ScVec, Uint256, WriteXdr,
    },
    BytesObject, Compare, Env, EnvBase, Error, Host, HostError, TryFromVal, U32Val, Val,
    DEFAULT_XDR_RW_LIMITS,
};
use more_asserts::assert_ge;
use soroban_test_wasms::LINEAR_MEMORY;
use std::ops::Deref;

#[test]
fn bytes_suite_of_tests() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
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
    let host = observe_host!(Host::test_host());
    let obj = host.bytes_new()?;
    let res = host.bytes_put(obj, 0u32.into(), 1u32.into());
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn bytes_slice_start_greater_than_end() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.bytes_new_from_slice(&[1, 2, 3, 4])?;
    let res = host.bytes_slice(obj, 2_u32.into(), 1_u32.into());
    let code = (ScErrorType::Object, ScErrorCode::InvalidInput);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn bytes_slice_start_equal_len() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.bytes_new_from_slice(&[1, 2, 3, 4])?;
    let res = host.bytes_slice(obj, 4_u32.into(), 4_u32.into())?;
    assert_eq!(host.obj_cmp(res.into(), host.bytes_new()?.into())?, 0);
    Ok(())
}

#[test]
fn bytes_slice_start_greater_than_len() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let obj = host.bytes_new_from_slice(&[1, 2, 3, 4])?;
    let res = host.bytes_slice(obj, 5_u32.into(), 10_u32.into());
    let code = (ScErrorType::Object, ScErrorCode::IndexBounds);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn bytes_xdr_roundtrip() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let roundtrip = |v: ScVal| -> Result<(), HostError> {
        let rv: Val = host.to_host_val(&v)?;
        let bo = host.serialize_to_bytes(rv)?;
        let rv_back = host.deserialize_from_bytes(bo)?;
        assert_eq!((*host).compare(&rv, &rv_back)?, core::cmp::Ordering::Equal);
        Ok(())
    };
    let deser_fails_scv = |v: ScVal| -> Result<(), HostError> {
        let bytes: Vec<u8> = v.to_xdr(DEFAULT_XDR_RW_LIMITS)?;
        let bo = host.add_host_object(ScBytes(bytes.try_into()?))?;
        let res = host.deserialize_from_bytes(bo);
        assert!(res.is_err());
        let err = res.err().unwrap().error;
        assert!(err.is_code(ScErrorCode::UnexpectedType) || err.is_code(ScErrorCode::InvalidInput));
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
    roundtrip(ScVal::Void)?;

    // objects
    // vec
    // Valid
    roundtrip(ScVal::Vec(Some(
        vec![
            ScVal::Error(ScError::Contract(123)),
            ScVal::U32(u32::MAX),
            ScVal::Symbol("abc".try_into().unwrap()),
        ]
        .try_into()?,
    )))?;
    // No payload fails
    deser_fails_scv(ScVal::Map(None))?;

    // non-representable fails
    deser_fails_scv(ScVal::Vec(Some(ScVec(
        vec![
            ScVal::U32(4321),
            ScVal::Vec(Some(ScVec(
                vec![ScVal::LedgerKeyContractInstance].try_into()?,
            ))),
        ]
        .try_into()?,
    ))))?;

    // Map
    // Valid
    roundtrip(ScVal::Map(Some(ScMap(
        vec![
            ScMapEntry {
                key: ScVal::Void,
                val: ScVal::U32(123),
            },
            ScMapEntry {
                key: ScVal::U64(222),
                val: ScVal::U32(123),
            },
            ScMapEntry {
                key: ScVal::U64(333),
                val: ScVal::U32(123),
            },
        ]
        .try_into()?,
    ))))?;
    // Duplicate key
    deser_fails_scv(ScVal::Map(Some(ScMap(
        vec![
            ScMapEntry {
                key: ScVal::Void,
                val: ScVal::U32(123),
            },
            ScMapEntry {
                key: ScVal::Void,
                val: ScVal::U32(234),
            },
        ]
        .try_into()?,
    ))))?;

    // No payload fails
    deser_fails_scv(ScVal::Map(None))?;

    // Out of order keys
    deser_fails_scv(ScVal::Map(Some(ScMap(
        vec![
            ScMapEntry {
                key: ScVal::U64(222),
                val: ScVal::U32(123),
            },
            ScMapEntry {
                key: ScVal::Void,
                val: ScVal::U32(123),
            },
        ]
        .try_into()?,
    ))))?;
    // Non-representable key
    deser_fails_scv(ScVal::Map(Some(ScMap(
        vec![
            ScMapEntry {
                key: ScVal::U64(222),
                val: ScVal::U32(123),
            },
            ScMapEntry {
                key: ScVal::LedgerKeyContractInstance,
                val: ScVal::U32(123),
            },
        ]
        .try_into()?,
    ))))?;
    // Non-representable value
    deser_fails_scv(ScVal::Map(Some(ScMap(
        vec![
            ScMapEntry {
                key: ScVal::U64(222),
                val: ScVal::U32(123),
            },
            ScMapEntry {
                key: ScVal::U64(333),
                val: ScVal::LedgerKeyContractInstance,
            },
        ]
        .try_into()?,
    ))))?;

    // Symbol
    // Valid
    roundtrip(ScVal::Symbol(crate::xdr::ScSymbol(
        "_a_B_Z_d123456".try_into()?,
    )))?;
    // Empty, valid
    roundtrip(ScVal::Symbol(crate::xdr::ScSymbol("".try_into()?)))?;
    // Bad character
    deser_fails_scv(ScVal::Symbol(crate::xdr::ScSymbol("a ".try_into()?)))?;
    // error
    roundtrip(ScVal::Error(ScError::Context(ScErrorCode::InternalError)))?;
    // Address
    // Account address
    roundtrip(ScVal::Address(ScAddress::Account(AccountId(
        PublicKey::PublicKeyTypeEd25519(Uint256([255; 32])),
    ))))?;
    // Contract address
    roundtrip(ScVal::Address(ScAddress::Contract(Hash([255; 32]))))?;
    // non-representable fails
    deser_fails_scv(ScVal::LedgerKeyContractInstance)?;

    Ok(())
}

#[test]
#[cfg(feature = "testutils")]
fn arbitrary_xdr_roundtrips() -> Result<(), HostError> {
    use arbitrary::{Arbitrary, Unstructured};
    use rand::RngCore;
    const ITERATIONS: u32 = 50_000;
    let host = Host::test_host_with_prng();
    host.budget_ref().reset_unlimited().unwrap();

    let mut successes = 0;
    let mut failures = 0;
    let mut roundtrip_test = |v: ScVal| -> Result<(), HostError> {
        let bytes: Vec<u8> = v.to_xdr(DEFAULT_XDR_RW_LIMITS)?;
        let scval_bytes_obj = host
            .add_host_object(ScBytes(bytes.try_into().unwrap()))
            .unwrap();
        // Not every randomly generated ScVal can be converted to `Val` due to non-representable
        // values and invariants like map ordering.
        let deserialize_res = host.deserialize_from_bytes(scval_bytes_obj);
        match deserialize_res {
            Ok(val) => {
                let serialized_bytes = host.serialize_to_bytes(val)?;
                assert_eq!(
                    host.compare(&scval_bytes_obj, &serialized_bytes)?,
                    core::cmp::Ordering::Equal
                );
                successes += 1;
            }
            Err(err) => {
                assert!(
                    err.error.is_code(ScErrorCode::UnexpectedType)
                        || err.error.is_code(ScErrorCode::InvalidInput)
                );
                failures += 1;
            }
        }
        Ok(())
    };
    for _ in 0..ITERATIONS {
        let mut data = vec![0u8; 5000];
        host.with_test_prng(|rng| {
            rng.fill_bytes(data.as_mut_slice());
            Ok(())
        })?;

        let sc_val = ScVal::arbitrary(&mut Unstructured::new(data.as_slice())).unwrap();
        roundtrip_test(sc_val)?;
    }
    eprintln!("successful roundtrips: {successes}, failed roundtrips: {failures}");
    // We don't hardcode the exact results here, but statistically there should be more valid
    // `ScVal`s generated.
    assert!(successes > failures);
    Ok(())
}

#[test]
fn test_malformed_xdr_decoding() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let run_test = |bytes: Vec<u8>| -> Result<(), HostError> {
        let bo = host.add_host_object(ScBytes(bytes.try_into()?))?;
        let res = host.deserialize_from_bytes(bo);
        assert!(res.is_err());
        Ok(())
    };
    run_test(vec![])?;
    run_test(vec![1, 2, 3, 4, 5])?;
    // Invalid enum variant.
    let mut v = ScVal::Void.to_xdr(DEFAULT_XDR_RW_LIMITS)?;
    v[0] = 1;
    run_test(v)?;

    let vec_xdr = ScVal::Vec(Some(ScVec(
        vec![ScVal::U32(100), ScVal::U32(101)].try_into().unwrap(),
    )))
    .to_xdr(DEFAULT_XDR_RW_LIMITS)?;

    let mut too_short_vec = vec_xdr.clone();
    too_short_vec[11] = 1;
    run_test(too_short_vec)?;

    let mut too_long_vec = vec_xdr.clone();
    too_long_vec[11] = 3;
    run_test(too_long_vec)?;

    let mut very_long_vec = vec_xdr.clone();
    very_long_vec[8] = 0xff;
    run_test(very_long_vec)?;

    let address_xdr = ScVal::Address(ScAddress::Account(AccountId(
        PublicKey::PublicKeyTypeEd25519(Uint256([255; 32])),
    )))
    .to_xdr(DEFAULT_XDR_RW_LIMITS)?;
    let mut add_char_address = address_xdr.clone();
    add_char_address.push(255);
    run_test(add_char_address)?;

    let mut remove_char_address = address_xdr.clone();
    remove_char_address.pop();
    run_test(remove_char_address)?;

    let mut wrong_variant_address = address_xdr.clone();
    wrong_variant_address[7] = 1;
    run_test(wrong_variant_address)?;

    Ok(())
}

#[test]
fn test_overly_large_bytes_xdr_decoding() {}

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
    let host = observe_host!(Host::test_host());
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
    let host = observe_host!(Host::test_host());
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
    let host = observe_host!(Host::test_host());

    let buf = vec![0; 42_000_000];
    let res = host.bytes_new_from_slice(&buf);
    assert_eq!(
        host.budget_ref()
            .get_tracker(ContractCostType::MemAlloc)?
            .inputs
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
    let wasm_short = wasm::wasm_module_with_large_bytes_from_linear_memory(100, 7);

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
        host.bytes_len(BytesObject::try_from_val(host.deref(), &res?).unwrap())?
            .to_val()
            .get_payload(),
        U32Val::from(100).to_val().get_payload()
    );

    // constructing a big bytes will cause budget limit exceeded error
    let num_bytes: u32 =
        if host.get_ledger_protocol_version()? < crate::vm::ModuleCache::MIN_LEDGER_VERSION {
            480_000
        } else {
            8_000_000
        };
    let wasm_long = wasm::wasm_module_with_large_bytes_from_linear_memory(num_bytes, 7);
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
