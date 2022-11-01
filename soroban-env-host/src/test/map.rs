use im_rc::OrdMap;
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
    assert!(!bool::try_from(host.map_has(obj.to_object(), k)?)?);
    let obj1 = host.map_put(obj.to_object(), k, v)?;
    assert!(bool::try_from(host.map_has(obj1, k)?)?);
    let rv = host.map_get(obj1, k)?;
    let v = unsafe { <u32 as RawValConvertible>::unchecked_from_val(rv) };
    assert_eq!(v, 6);
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
        assert!(host
            .map_prev_key(obj.to_object(), 0_u32.into())?
            .shallow_eq(&Status::UNKNOWN_ERROR.to_raw()));
        assert!(host
            .map_prev_key(obj.to_object(), 1_u32.into())?
            .shallow_eq(&Status::UNKNOWN_ERROR.to_raw()));
        assert!(host
            .map_prev_key(obj.to_object(), 2_u32.into())?
            .shallow_eq(&RawVal::from_u32(1)));
        assert!(host
            .map_prev_key(obj.to_object(), 4_u32.into())?
            .shallow_eq(&RawVal::from_u32(1)));
        assert!(host
            .map_prev_key(obj.to_object(), 5_u32.into())?
            .shallow_eq(&RawVal::from_u32(4)));
    }
    // next
    {
        assert!(host
            .map_next_key(obj.to_object(), 5_u32.into())?
            .shallow_eq(&Status::UNKNOWN_ERROR.to_raw()));
        assert!(host
            .map_next_key(obj.to_object(), 4_u32.into())?
            .shallow_eq(&Status::UNKNOWN_ERROR.to_raw()));
        assert!(host
            .map_next_key(obj.to_object(), 3_u32.into())?
            .shallow_eq(&RawVal::from_u32(4)));
        assert!(host
            .map_next_key(obj.to_object(), 1_u32.into())?
            .shallow_eq(&RawVal::from_u32(4)));
        assert!(host
            .map_next_key(obj.to_object(), 0_u32.into())?
            .shallow_eq(&RawVal::from_u32(1)));
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
        assert!(host
            .map_prev_key(test_map, 0_u32.into())?
            .shallow_eq(&Status::UNKNOWN_ERROR.to_raw()));
        assert!(host
            .map_prev_key(test_map, 4_u32.into())?
            .shallow_eq(&RawVal::from_u32(2)));
        assert!(host
            .map_prev_key(test_map, obj_vec.clone().into())?
            .shallow_eq(&RawVal::from_u32(2)));
        assert!(host
            .map_prev_key(test_map, obj_map.clone().into())?
            .shallow_eq(&obj_vec.to_raw()));
        assert!(host
            .map_prev_key(test_map, sym.clone().into())?
            .shallow_eq(&obj_map.to_raw()));
    }
    // next
    {
        assert!(host
            .map_next_key(test_map, 0_u32.into())?
            .shallow_eq(&RawVal::from_u32(2)));
        assert!(host
            .map_next_key(test_map, 4_u32.into())?
            .shallow_eq(&obj_vec.to_raw()));
        assert!(host
            .map_next_key(test_map, obj_vec.clone().into())?
            .shallow_eq(&obj_map.to_raw()));
        assert!(host
            .map_next_key(test_map, obj_map.clone().into())?
            .shallow_eq(&sym.to_raw()));
        assert!(host
            .map_next_key(test_map, sym.clone().into())?
            .shallow_eq(&Status::UNKNOWN_ERROR.to_raw()));
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

    assert_eq!(host.obj_cmp(keys, expected_keys.to_object())?, 0);

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

    assert_eq!(host.obj_cmp(values, expected_values.to_object())?, 0);

    Ok(())
}

#[test]
#[ignore = "aborts with a stack overflow"]
fn map_stack_overflow_63356_big_keys_and_vals() {
    let mut map: OrdMap<LedgerKey, Option<LedgerEntry>> = OrdMap::new();
    for a in 0..=255 {
        for b in 0..=255 {
            let mut k: [u8; 32] = [0; 32];
            k[0] = a;
            k[1] = b;
            let pk = PublicKey::PublicKeyTypeEd25519(Uint256(k));
            let key = LedgerKey::Account(LedgerKeyAccount {
                account_id: AccountId(pk),
            });
            map.insert(key, None);
        }
    }
}

#[test]
fn map_stack_no_overflow_65536_boxed_keys_and_vals() {
    let mut map: OrdMap<Box<LedgerKey>, Option<Box<LedgerEntry>>> = OrdMap::new();
    for a in 0..=255 {
        for b in 0..=255 {
            let mut k: [u8; 32] = [0; 32];
            k[0] = a;
            k[1] = b;
            let pk = PublicKey::PublicKeyTypeEd25519(Uint256(k));
            let key = LedgerKey::Account(LedgerKeyAccount {
                account_id: AccountId(pk),
            });
            map.insert(Box::new(key), None);
        }
    }
}

#[test]
#[ignore = "runs for too long on debug builds"]
fn map_stack_no_overflow_16777216_boxed_keys_and_vals() {
    let mut map: OrdMap<Box<LedgerKey>, Option<Box<LedgerEntry>>> = OrdMap::new();
    for a in 0..=255 {
        for b in 0..=255 {
            for c in 0..=255 {
                let mut k: [u8; 32] = [0; 32];
                k[0] = a;
                k[1] = b;
                k[2] = c;
                let pk = PublicKey::PublicKeyTypeEd25519(Uint256(k));
                let key = LedgerKey::Account(LedgerKeyAccount {
                    account_id: AccountId(pk),
                });
                map.insert(Box::new(key), None);
            }
        }
    }
}
