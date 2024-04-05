use crate::{
    testutils::wasm,
    xdr::{
        AccountId, ContractCostType, LedgerEntry, LedgerKey, LedgerKeyAccount, PublicKey,
        ScErrorCode, ScErrorType, ScMap, ScMapEntry, ScVal, ScVec, Uint256, VecM,
    },
    Env, Error, Host, HostError, MapObject, MeteredOrdMap, Symbol, SymbolSmall, TryFromVal, U32Val,
    Val,
};
use more_asserts::assert_ge;
use soroban_test_wasms::LINEAR_MEMORY;
use std::{ops::Deref, rc::Rc, time::Instant};

const MAP_OOB: Error = Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds);

#[test]
fn map_put_has_and_get() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let scmap: ScMap = host.map_err(
        vec![
            ScMapEntry {
                key: ScVal::U32(1),
                val: ScVal::U32(2),
            },
            ScMapEntry {
                key: ScVal::U32(2),
                val: ScVal::U32(4),
            },
        ]
        .try_into(),
    )?;
    let obj: MapObject = host.to_host_val(&ScVal::Map(Some(scmap)))?.try_into()?;
    let k: Val = 3_u32.into();
    let v: Val = 6_u32.into();
    assert!(!bool::try_from(host.map_has(obj, k)?)?);
    let obj1 = host.map_put(obj, k, v)?;
    assert!(bool::try_from(host.map_has(obj1, k)?)?);
    let rv = host.map_get(obj1, k)?;
    let v: u32 = rv.try_into()?;
    assert_eq!(v, 6);
    Ok(())
}

#[test]
fn map_put_insert_and_remove() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let scmap: ScMap = host.map_err(
        vec![
            ScMapEntry {
                key: ScVal::U32(1),
                val: ScVal::U32(10),
            },
            ScMapEntry {
                key: ScVal::U32(3),
                val: ScVal::U32(30),
            },
        ]
        .try_into(),
    )?;
    let scobj = ScVal::Map(Some(scmap));
    let mut obj: MapObject = host.to_host_val(&scobj)?.try_into()?;

    obj = host.map_put(obj, 0_u32.into(), 0_u32.into())?;
    obj = host.map_put(obj, 2_u32.into(), 20_u32.into())?;
    obj = host.map_put(obj, 4_u32.into(), 40_u32.into())?;

    let scmap_r: ScMap = host.map_err(
        vec![
            ScMapEntry {
                key: ScVal::U32(0),
                val: ScVal::U32(0),
            },
            ScMapEntry {
                key: ScVal::U32(1),
                val: ScVal::U32(10),
            },
            ScMapEntry {
                key: ScVal::U32(2),
                val: ScVal::U32(20),
            },
            ScMapEntry {
                key: ScVal::U32(3),
                val: ScVal::U32(30),
            },
            ScMapEntry {
                key: ScVal::U32(4),
                val: ScVal::U32(40),
            },
        ]
        .try_into(),
    )?;
    let obj_r: MapObject = host.to_host_val(&ScVal::Map(Some(scmap_r)))?.try_into()?;
    assert_eq!(host.obj_cmp(obj_r.into(), obj.into())?, 0);

    obj = host.map_del(obj, 0_u32.into())?;
    obj = host.map_del(obj, 2_u32.into())?;
    obj = host.map_del(obj, 4_u32.into())?;
    let obj_o: MapObject = host.to_host_val(&scobj)?.try_into()?;
    assert_eq!(host.obj_cmp(obj_o.into(), obj.into())?, 0);
    Ok(())
}

#[test]
fn map_iter_by_index() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let scmap: ScMap = host.map_err(
        vec![
            ScMapEntry {
                key: ScVal::U32(1),
                val: ScVal::U32(2),
            },
            ScMapEntry {
                key: ScVal::U32(4),
                val: ScVal::U32(8),
            },
        ]
        .try_into(),
    )?;
    let scobj = ScVal::Map(Some(scmap));
    let obj: MapObject = host.to_host_val(&scobj)?.try_into()?;
    // get keys
    assert_eq!(
        host.map_key_by_pos(obj, U32Val::from(0))?.get_payload(),
        U32Val::from(1).to_val().get_payload()
    );
    assert_eq!(
        host.map_key_by_pos(obj, U32Val::from(1))?.get_payload(),
        U32Val::from(4).to_val().get_payload()
    );
    assert!(HostError::result_matches_err(
        host.map_key_by_pos(obj, U32Val::from(2)),
        MAP_OOB
    ));
    // get vals
    assert_eq!(
        host.map_val_by_pos(obj, U32Val::from(0))?.get_payload(),
        U32Val::from(2).to_val().get_payload()
    );
    assert_eq!(
        host.map_val_by_pos(obj, U32Val::from(1))?.get_payload(),
        U32Val::from(8).to_val().get_payload()
    );
    assert!(HostError::result_matches_err(
        host.map_val_by_pos(obj, U32Val::from(2)),
        MAP_OOB
    ));

    Ok(())
}

#[test]
fn hetro_map_iter_by_index() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let scmap: ScMap = host.map_err(
        vec![ScMapEntry {
            key: ScVal::U32(1),
            val: ScVal::U32(2),
        }]
        .try_into(),
    )?;
    let scvec: ScVec = host.test_scvec::<u32>(&[1])?;

    let scobj_map = ScVal::Map(Some(scmap));
    let scobj_vec = ScVal::Vec(Some(scvec));
    let sym = Symbol::try_from_small_str("symbol").unwrap();

    let obj_map = host.to_host_val(&scobj_map)?;
    let obj_vec = host.to_host_val(&scobj_vec)?;

    let mut test_map = host.map_new()?;
    test_map = host.map_put(test_map, 2u32.into(), 1u32.into())?;
    test_map = host.map_put(test_map, obj_map, 2u32.into())?;
    test_map = host.map_put(test_map, obj_vec, 3u32.into())?;
    test_map = host.map_put(test_map, sym.into(), 4u32.into())?;
    // The key ordering should be [u32, symbol, vec, map]
    {
        assert!(HostError::result_matches_err(
            host.map_key_by_pos(test_map, U32Val::from(4)),
            MAP_OOB
        ));
        assert_eq!(
            host.map_key_by_pos(test_map, U32Val::from(0))?
                .get_payload(),
            Val::from_u32(2).to_val().get_payload()
        );
        assert_eq!(
            host.map_key_by_pos(test_map, U32Val::from(1))?
                .get_payload(),
            sym.as_val().get_payload()
        );
        assert_eq!(
            host.map_key_by_pos(test_map, U32Val::from(2))?
                .get_payload(),
            obj_vec.get_payload()
        );
        assert_eq!(
            host.map_key_by_pos(test_map, U32Val::from(3))?
                .get_payload(),
            obj_map.get_payload()
        );
    }
    // The val ordering should be [1u32, 4u32, 3u32, 2u32]
    {
        assert!(HostError::result_matches_err(
            host.map_val_by_pos(test_map, U32Val::from(4)),
            MAP_OOB
        ));
        assert_eq!(
            host.map_val_by_pos(test_map, U32Val::from(0))?
                .get_payload(),
            Val::from_u32(1).to_val().get_payload()
        );
        assert_eq!(
            host.map_val_by_pos(test_map, U32Val::from(1))?
                .get_payload(),
            Val::from_u32(4).to_val().get_payload()
        );
        assert_eq!(
            host.map_val_by_pos(test_map, U32Val::from(2))?
                .get_payload(),
            Val::from_u32(3).to_val().get_payload()
        );
        assert_eq!(
            host.map_val_by_pos(test_map, U32Val::from(3))?
                .get_payload(),
            Val::from_u32(2).to_val().get_payload()
        );
    }

    Ok(())
}

#[test]
fn map_keys() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    let mut map = host.map_new()?;
    map = host.map_put(map, 2u32.into(), 20u32.into())?;
    map = host.map_put(map, 1u32.into(), 10u32.into())?;
    let keys = host.map_keys(map)?;

    let expected_keys = host.test_vec_obj::<u32>(&[1, 2])?;

    assert_eq!(host.obj_cmp(keys.try_into()?, expected_keys.into())?, 0);

    Ok(())
}

#[test]
fn map_values() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    let mut map = host.map_new()?;
    map = host.map_put(map, 2u32.into(), 20u32.into())?;
    map = host.map_put(map, 1u32.into(), 10u32.into())?;
    let values = host.map_values(map)?;

    let expected_values = host.test_vec_obj::<u32>(&[10, 20])?;

    assert_eq!(host.obj_cmp(values.into(), expected_values.into())?, 0);

    Ok(())
}

#[test]
fn map_stack_no_overflow_65536_boxed_keys_and_vals() {
    let mut map: Vec<(Rc<LedgerKey>, Option<Rc<LedgerEntry>>)> = Vec::new();
    for a in 0..=255 {
        for b in 0..=255 {
            let mut k: [u8; 32] = [0; 32];
            k[0] = a;
            k[1] = b;
            let pk = PublicKey::PublicKeyTypeEd25519(Uint256(k));
            let key = LedgerKey::Account(LedgerKeyAccount {
                account_id: AccountId(pk),
            });
            map.push((Rc::new(key), None));
        }
    }
}

#[test]
fn scmap_out_of_order() {
    let host = observe_host!(Host::test_host());
    let bad_scmap = ScVal::Map(Some(ScMap(
        VecM::try_from(vec![
            ScMapEntry {
                key: ScVal::U32(2),
                val: ScVal::U32(0),
            },
            ScMapEntry {
                key: ScVal::U32(3),
                val: ScVal::U32(0),
            },
            ScMapEntry {
                key: ScVal::U32(1),
                val: ScVal::U32(0),
            },
        ])
        .unwrap(),
    )));
    assert!(Val::try_from_val(&*host, &bad_scmap).is_err());
}

#[test]
fn scmap_duplicate() {
    let host = observe_host!(Host::test_host());
    let bad_scmap = ScVal::Map(Some(ScMap(
        VecM::try_from(vec![
            ScMapEntry {
                key: ScVal::U32(2),
                val: ScVal::U32(0),
            },
            ScMapEntry {
                key: ScVal::U32(2),
                val: ScVal::U32(0),
            },
        ])
        .unwrap(),
    )));
    assert!(Val::try_from_val(&*host, &bad_scmap).is_err());
}

#[test]
fn scmap_invalid_element() {
    let host = observe_host!(Host::test_host());
    let bad_nested_scmap = ScVal::Map(Some(ScMap(
        VecM::try_from(vec![
            ScMapEntry {
                key: ScVal::U32(2),
                val: ScVal::U32(0),
            },
            ScMapEntry {
                key: ScVal::U32(2),
                val: ScVal::U32(0),
            },
        ])
        .unwrap(),
    )));

    let bad_scmap = ScVal::Map(Some(ScMap(
        VecM::try_from(vec![ScMapEntry {
            key: ScVal::U32(2),
            val: bad_nested_scmap,
        }])
        .unwrap(),
    )));

    assert!(Val::try_from_val(&*host, &bad_scmap).is_err());
}

#[test]
fn map_build_bad_element_integrity() -> Result<(), HostError> {
    use crate::EnvBase;
    let host = observe_host!(Host::test_host());
    let obj = host.map_new()?;

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

    // Inserting ok object referejces into maps should work.
    assert!(host.map_put(obj, ok_val, ok_val).is_ok());
    assert!(host.map_new_from_slices(&["hi"], &[ok_val]).is_ok());

    // Inserting corrupt object references into maps should fail.
    assert!(host.map_put(obj, ok_val, bad_tag).is_err());
    assert!(host.map_put(obj, bad_tag, ok_val).is_err());
    assert!(host.map_new_from_slices(&["hi"], &[bad_tag]).is_err());

    assert!(host.map_put(obj, ok_val, bad_handle).is_err());
    assert!(host.map_put(obj, bad_handle, ok_val).is_err());
    assert!(host.map_new_from_slices(&["hi"], &[bad_handle]).is_err());

    Ok(())
}

#[test]
fn large_map_exceeds_budget() {
    let host = Host::default();
    // Set a fixed budget higher than defaults, but still realistic.
    const MEMORY_LIMIT: u64 = 200 * 1024 * 1024;
    host.budget_ref()
        .reset_limits(200_000_000, MEMORY_LIMIT)
        .unwrap();
    let start = Instant::now();
    let mut m = host.map_new().unwrap();
    let mut i = 0;
    loop {
        i += 1;
        let new_m_res = host.map_put(m, U32Val::from(u32::MAX - i).into(), U32Val::from(i).into());
        match new_m_res {
            Ok(new_m) => {
                m = new_m;
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
fn initialization_invalid() -> Result<(), HostError> {
    use crate::EnvBase;
    let host = observe_host!(Host::test_host());

    // Out of order keys
    assert!(host
        .map_new_from_slices(&["b", "a"], &[1u32.into(), 2u32.into()])
        .is_err());

    // Duplicate
    assert!(host
        .map_new_from_slices(&["a", "a"], &[1u32.into(), 2u32.into()])
        .is_err());

    // large map key
    let key: String = ('0'..'9').chain('a'..'z').collect();
    let buf = vec![1; 32];
    let scv_bytes = ScVal::Bytes(buf.try_into().unwrap());
    let val = host.to_host_val(&scv_bytes)?;
    let res = host.map_new_from_slices(&[key.as_str()], &[val]);
    // conversion error from symbol
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Value, ScErrorCode::InvalidInput)
    ));

    // large map from slices
    let buf = vec![0; 7_000_000];
    let scv_bytes = ScVal::Bytes(buf.try_into().unwrap());
    let bytes_val = host.to_host_val(&scv_bytes)?;

    let vals = vec![bytes_val; 5];
    let keys = ["a", "b", "c", "d", "e"];

    let map = host.map_new_from_slices(&keys, &vals.as_slice())?;
    let res = host.from_host_val(map.to_val());
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));

    Ok(())
}

#[test]
fn instantiate_oversized_map_from_linear_memory() -> Result<(), HostError> {
    let wasm_short =
        wasm::wasm_module_with_large_map_from_linear_memory(100, U32Val::from(7).to_val());

    // sanity check, constructing a short map is ok
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id_obj = host.register_test_contract_wasm(wasm_short.as_slice());
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("test")?,
        host.test_vec_obj::<u32>(&[])?,
    );
    assert!(res.is_ok());
    assert_eq!(
        host.map_len(MapObject::try_from_val(host.deref(), &res?).unwrap())?
            .to_val()
            .get_payload(),
        U32Val::from(100).to_val().get_payload()
    );

    // constructing a big map will cause budget limit exceeded error
    let num_vals =
        if host.get_ledger_protocol_version()? < crate::vm::ModuleCache::MIN_LEDGER_VERSION {
            20_000
        } else {
            400_000
        };
    let wasm_long =
        wasm::wasm_module_with_large_map_from_linear_memory(num_vals, U32Val::from(7).to_val());
    host.clear_module_cache()?;
    host.budget_ref().reset_unlimited()?;
    let contract_id_obj2 = host.register_test_contract_wasm(&wasm_long.as_slice());
    host.budget_ref().reset_default()?;

    // We want to observe a failure that happens part way through contract
    // instantiation, which means the weird half-deferred charging of
    // module-cache construct we do in storage-recording mode will interfere
    // with this test in feature=testutils mode (which turns on
    // feature=recording_mode implicitly). The easiest workaround is just to
    // switch to enforcing mode.
    if host.get_ledger_protocol_version()? >= crate::vm::ModuleCache::MIN_LEDGER_VERSION {
        host.switch_to_enforcing_storage()?;
    }

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

#[test]
fn metered_map_initialization() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let k1 = SymbolSmall::try_from_str("a")?.to_val();
    let k2 = SymbolSmall::try_from_str("b")?.to_val();
    let v1 = U32Val::from(1).to_val();
    let v2 = U32Val::from(2).to_val();
    let v = vec![(k1, v1), (k2, v2)];
    let m = MeteredOrdMap::from_map(v.clone(), host.deref());
    assert!(m.is_ok());
    let m = MeteredOrdMap::from_exact_iter(v.into_iter(), host.deref());
    assert!(m.is_ok());

    // out of order keys
    let v = vec![(k2, v1), (k1, v2)];
    let m = MeteredOrdMap::from_map(v.clone(), host.deref());
    assert!(HostError::result_matches_err(
        m,
        (ScErrorType::Object, ScErrorCode::InvalidInput)
    ));
    let m = MeteredOrdMap::from_exact_iter(v.into_iter(), host.deref());
    assert!(HostError::result_matches_err(
        m,
        (ScErrorType::Object, ScErrorCode::InvalidInput)
    ));

    // duplicate keys
    let v = vec![(k1, v1), (k1, v2)];
    let m = MeteredOrdMap::from_map(v.clone(), host.deref());
    assert!(HostError::result_matches_err(
        m,
        (ScErrorType::Object, ScErrorCode::InvalidInput)
    ));
    let m = MeteredOrdMap::from_exact_iter(v.into_iter(), host.deref());
    assert!(HostError::result_matches_err(
        m,
        (ScErrorType::Object, ScErrorCode::InvalidInput)
    ));

    Ok(())
}

#[test]
fn linear_memory_operations() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.enable_debug()?;
    let id_obj = host.register_test_contract_wasm(LINEAR_MEMORY);

    {
        let args = host.vec_new()?;
        let res = host.call(
            id_obj,
            Symbol::try_from_val(&*host, &"map_mem_ok").unwrap(),
            args,
        );

        assert!(res.is_ok());
    }

    {
        let args = host.vec_new()?;
        let res = host.call(
            id_obj,
            Symbol::try_from_val(&*host, &"map_keys_out_of_order").unwrap(),
            args,
        );
        assert!(HostError::result_matches_err(
            res,
            (ScErrorType::Object, ScErrorCode::InvalidInput)
        ));
    }

    {
        let args = host.vec_new()?;
        let res = host.call(
            id_obj,
            Symbol::try_from_val(&*host, &"map_duplicate_keys").unwrap(),
            args,
        );
        assert!(HostError::result_matches_err(
            res,
            (ScErrorType::Object, ScErrorCode::InvalidInput)
        ));
    }
    Ok(())
}
