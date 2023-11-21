use std::rc::Rc;

use soroban_env_common::{
    xdr::{
        AccountId, LedgerEntry, LedgerKey, LedgerKeyAccount, PublicKey, ScErrorCode, ScErrorType,
        Uint256,
    },
    MapObject, U32Val,
};
use soroban_test_wasms::LINEAR_MEMORY;

use crate::{
    xdr::{ScMap, ScMapEntry, ScVal, ScVec, VecM},
    Env, Error, Host, HostError, Symbol, TryFromVal, Val,
};

const MAP_OOB: Error = Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds);

#[test]
fn map_put_has_and_get() -> Result<(), HostError> {
    let host = observe_host!(Host::default());
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
    let host = observe_host!(Host::default());
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
    let host = observe_host!(Host::default());
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
    let host = observe_host!(Host::default());
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
    let host = observe_host!(Host::default());

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
    let host = observe_host!(Host::default());

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
    let host = observe_host!(Host::default());
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
fn map_build_bad_element_integrity() -> Result<(), HostError> {
    use crate::EnvBase;
    let host = observe_host!(Host::default());
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
fn initialization_invalid() -> Result<(), HostError> {
    use crate::EnvBase;
    let host = observe_host!(Host::default());

    // Out of order keys
    assert!(host
        .map_new_from_slices(&["b", "a"], &[1u32.into(), 2u32.into()])
        .is_err());

    // Duplicate
    assert!(host
        .map_new_from_slices(&["a", "a"], &[1u32.into(), 2u32.into()])
        .is_err());

    let buf = vec![0; 7000000];
    let scv_bytes = ScVal::Bytes(buf.try_into().unwrap());
    let bytes_val = host.to_host_val(&scv_bytes)?;

    // roughly consumes 7*5=42MiB (> 40 MiB is the budget limit)
    let vals = vec![bytes_val; 5];
    let keys = ["a", "b", "c", "d", "e"];

    let res = host.map_new_from_slices(&keys, &vals.as_slice())?;
    assert!(host.from_host_val(res.to_val()).is_err());

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
            Symbol::try_from_val(&*host, &"map_mem_bad").unwrap(),
            args,
        );
        assert!(res.is_err());
        assert!(res.unwrap_err().error.is_code(ScErrorCode::InvalidInput));
    }
    Ok(())
}
