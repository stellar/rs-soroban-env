use std::rc::Rc;

use soroban_env_common::xdr::{
    AccountId, LedgerEntry, LedgerKey, LedgerKeyAccount, PublicKey, Uint256,
};

use crate::{
    xdr::{ScMap, ScMapEntry, ScObject, ScVal, ScVec},
    CheckedEnv, Host, HostError, RawVal, RawValConvertible, Status, Symbol,
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
    let scobj = ScObject::Map(scmap);
    let obj = host.to_host_obj(&scobj)?;
    let k: RawVal = 3_u32.into();
    let v: RawVal = 6_u32.into();
    assert!(!bool::try_from(host.map_has(obj, k)?)?);
    let obj1 = host.map_put(obj, k, v)?;
    assert!(bool::try_from(host.map_has(obj1, k)?)?);
    let rv = host.map_get(obj1, k)?;
    let v = unsafe { <u32 as RawValConvertible>::unchecked_from_val(rv) };
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
    let scobj = ScObject::Map(scmap);
    let mut obj = host.to_host_obj(&scobj)?;

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
    let scobj_r = ScObject::Map(scmap_r);
    let obj_r = host.to_host_obj(&scobj_r)?;
    assert_eq!(host.obj_cmp(obj_r.into(), obj.into())?, 0);

    obj = host.map_del(obj, 0_u32.into())?;
    obj = host.map_del(obj, 2_u32.into())?;
    obj = host.map_del(obj, 4_u32.into())?;
    let obj_o = host.to_host_obj(&scobj)?;
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
    let scobj = ScObject::Map(scmap);
    let obj = host.to_host_obj(&scobj)?;
    // prev
    {
        assert_eq!(
            host.map_prev_key(obj, 0_u32.into())?.get_payload(),
            Status::UNKNOWN_ERROR.to_raw().get_payload()
        );
        assert_eq!(
            host.map_prev_key(obj, 1_u32.into())?.get_payload(),
            Status::UNKNOWN_ERROR.to_raw().get_payload()
        );
        assert_eq!(
            host.map_prev_key(obj, 2_u32.into())?.get_payload(),
            RawVal::from_u32(1).get_payload()
        );
        assert_eq!(
            host.map_prev_key(obj, 4_u32.into())?.get_payload(),
            RawVal::from_u32(1).get_payload()
        );
        assert_eq!(
            host.map_prev_key(obj, 5_u32.into())?.get_payload(),
            RawVal::from_u32(4).get_payload()
        );
    }
    // next
    {
        assert_eq!(
            host.map_next_key(obj, 5_u32.into())?.get_payload(),
            Status::UNKNOWN_ERROR.to_raw().get_payload()
        );
        assert_eq!(
            host.map_next_key(obj, 4_u32.into())?.get_payload(),
            Status::UNKNOWN_ERROR.to_raw().get_payload()
        );
        assert_eq!(
            host.map_next_key(obj, 3_u32.into())?.get_payload(),
            RawVal::from_u32(4).get_payload()
        );
        assert_eq!(
            host.map_next_key(obj, 1_u32.into())?.get_payload(),
            RawVal::from_u32(4).get_payload()
        );
        assert_eq!(
            host.map_next_key(obj, 0_u32.into())?.get_payload(),
            RawVal::from_u32(1).get_payload()
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

    let scobj_map = ScObject::Map(scmap);
    let scobj_vec = ScObject::Vec(scvec);
    let sym = Symbol::from_str("symbol");

    let obj_map = host.to_host_obj(&scobj_map)?;
    let obj_vec = host.to_host_obj(&scobj_vec)?;

    let mut test_map = host.map_new()?;
    test_map = host.map_put(test_map, 2u32.into(), 4u32.into())?;
    test_map = host.map_put(test_map, obj_map.clone().into(), 4u32.into())?;
    test_map = host.map_put(test_map, obj_vec.clone().into(), 4u32.into())?;
    test_map = host.map_put(test_map, sym.clone().into(), 4u32.into())?;
    // The key ordering should be [u32, vec, map, symbol]
    // prev
    {
        assert_eq!(
            host.map_prev_key(test_map, 0_u32.into())?.get_payload(),
            Status::UNKNOWN_ERROR.to_raw().get_payload()
        );
        assert_eq!(
            host.map_prev_key(test_map, 4_u32.into())?.get_payload(),
            RawVal::from_u32(2).get_payload()
        );
        assert_eq!(
            host.map_prev_key(test_map, obj_vec.clone().into())?
                .get_payload(),
            RawVal::from_u32(2).get_payload()
        );
        assert_eq!(
            host.map_prev_key(test_map, obj_map.clone().into())?
                .get_payload(),
            obj_vec.to_raw().get_payload()
        );
        assert_eq!(
            host.map_prev_key(test_map, sym.clone().into())?
                .get_payload(),
            obj_map.to_raw().get_payload()
        );
    }
    // next
    {
        assert_eq!(
            host.map_next_key(test_map, 0_u32.into())?.get_payload(),
            RawVal::from_u32(2).get_payload()
        );
        assert_eq!(
            host.map_next_key(test_map, 4_u32.into())?.get_payload(),
            obj_vec.to_raw().get_payload()
        );
        assert_eq!(
            host.map_next_key(test_map, obj_vec.clone().into())?
                .get_payload(),
            obj_map.to_raw().get_payload()
        );
        assert_eq!(
            host.map_next_key(test_map, obj_map.clone().into())?
                .get_payload(),
            sym.to_raw().get_payload()
        );
        assert_eq!(
            host.map_next_key(test_map, sym.clone().into())?
                .get_payload(),
            Status::UNKNOWN_ERROR.to_raw().get_payload()
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

    let expected_keys = host.test_vec_obj::<u32>(&[1, 2])?.to_raw();

    assert_eq!(host.obj_cmp(keys.to_raw(), expected_keys)?, 0);

    Ok(())
}

#[test]
fn map_values() -> Result<(), HostError> {
    let host = Host::default();

    let mut map = host.map_new()?;
    map = host.map_put(map, 2u32.into(), 20u32.into())?;
    map = host.map_put(map, 1u32.into(), 10u32.into())?;
    let values = host.map_values(map)?;

    let expected_values = host.test_vec_obj::<u32>(&[10, 20])?.to_raw();

    assert_eq!(host.obj_cmp(values.to_raw(), expected_values)?, 0);

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
