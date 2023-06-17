use std::rc::Rc;

use soroban_env_common::{
    xdr::{
        AccountId, LedgerEntry, LedgerKey, LedgerKeyAccount, PublicKey, ScErrorCode, ScErrorType,
        Uint256,
    },
    MapObject,
};

use crate::{
    xdr::{ScMap, ScMapEntry, ScVal, ScVec},
    Env, Error, Host, HostError, Symbol, Val,
};

#[test]
fn map_put_has_and_get() -> Result<(), HostError> {
    let host = Host::default();
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
    let host = Host::default();
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
fn map_prev_and_next() -> Result<(), HostError> {
    let host = Host::default();
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
    // prev
    {
        assert_eq!(
            host.map_prev_key(obj, 0_u32.into())?.get_payload(),
            Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds)
                .to_val()
                .get_payload()
        );
        assert_eq!(
            host.map_prev_key(obj, 1_u32.into())?.get_payload(),
            Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds)
                .to_val()
                .get_payload()
        );
        assert_eq!(
            host.map_prev_key(obj, 2_u32.into())?.get_payload(),
            Val::from_u32(1).to_val().get_payload()
        );
        assert_eq!(
            host.map_prev_key(obj, 4_u32.into())?.get_payload(),
            Val::from_u32(1).to_val().get_payload()
        );
        assert_eq!(
            host.map_prev_key(obj, 5_u32.into())?.get_payload(),
            Val::from_u32(4).to_val().get_payload()
        );
    }
    // next
    {
        assert_eq!(
            host.map_next_key(obj, 5_u32.into())?.get_payload(),
            Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds)
                .to_val()
                .get_payload()
        );
        assert_eq!(
            host.map_next_key(obj, 4_u32.into())?.get_payload(),
            Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds)
                .to_val()
                .get_payload()
        );
        assert_eq!(
            host.map_next_key(obj, 3_u32.into())?.get_payload(),
            Val::from_u32(4).to_val().get_payload()
        );
        assert_eq!(
            host.map_next_key(obj, 1_u32.into())?.get_payload(),
            Val::from_u32(4).to_val().get_payload()
        );
        assert_eq!(
            host.map_next_key(obj, 0_u32.into())?.get_payload(),
            Val::from_u32(1).to_val().get_payload()
        );
    }
    Ok(())
}

#[test]
fn map_prev_and_next_heterogeneous() -> Result<(), HostError> {
    let host = Host::default();
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
    test_map = host.map_put(test_map, 2u32.into(), 4u32.into())?;
    test_map = host.map_put(test_map, obj_map, 4u32.into())?;
    test_map = host.map_put(test_map, obj_vec, 4u32.into())?;
    test_map = host.map_put(test_map, sym.into(), 4u32.into())?;
    // The key ordering should be [u32, symbol, vec, map]
    // prev
    {
        assert_eq!(
            host.map_prev_key(test_map, 0_u32.into())?.get_payload(),
            Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds)
                .to_val()
                .get_payload()
        );
        assert_eq!(
            host.map_prev_key(test_map, 4_u32.into())?.get_payload(),
            Val::from_u32(2).to_val().get_payload()
        );
        assert_eq!(
            host.map_prev_key(test_map, sym.into())?.get_payload(),
            Val::from_u32(2).to_val().get_payload()
        );
        assert_eq!(
            host.map_prev_key(test_map, obj_vec)?.get_payload(),
            sym.as_val().get_payload()
        );
        assert_eq!(
            host.map_prev_key(test_map, obj_map)?.get_payload(),
            obj_vec.get_payload()
        );
    }
    // next
    {
        assert_eq!(
            host.map_next_key(test_map, 0_u32.into())?.get_payload(),
            Val::from_u32(2).to_val().get_payload()
        );
        assert_eq!(
            host.map_next_key(test_map, 4_u32.into())?.get_payload(),
            sym.as_val().get_payload()
        );
        assert_eq!(
            host.map_next_key(test_map, sym.into())?.get_payload(),
            obj_vec.get_payload()
        );
        assert_eq!(
            host.map_next_key(test_map, obj_vec)?.get_payload(),
            obj_map.get_payload()
        );
        assert_eq!(
            host.map_next_key(test_map, obj_map)?.get_payload(),
            Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds)
                .to_val()
                .get_payload()
        );
    }

    Ok(())
}

#[test]
fn map_keys() -> Result<(), HostError> {
    let host = Host::default();

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
    let host = Host::default();

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
