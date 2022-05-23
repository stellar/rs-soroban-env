use stellar_contract_env_common::RawValConvertible;

use crate::{
    xdr::{ScObject, ScObjectType, ScVal, ScVec},
    Host, IntoEnvVal, Object, Tag,
};

#[test]
fn u64_roundtrip() {
    let host = Host::default();
    let u: u64 = 38473_u64; // This will be treated as a ScVal::I64
    let v = u.into_env_val(&host);
    let j = u64::try_from(v).unwrap();
    assert_eq!(u, j);

    let u2: u64 = u64::MAX; // This will be treated as ScVal::Object::U64
    let v2 = u2.into_env_val(&host);
    let obj: Object = v2.val.try_into().unwrap();
    assert!(obj.is_obj_type(ScObjectType::U64));
    assert_eq!(obj.get_handle(), 0);
    let k = u64::try_from(v2).unwrap();
    assert_eq!(u2, k);
}

#[test]
fn i64_roundtrip() {
    let host = Host::default();
    let i: i64 = 12345_i64; // Will be treated as ScVal::I64
    let v = i.into_env_val(&host);
    let j = i64::try_from(v).unwrap();
    assert_eq!(i, j);

    let i2: i64 = -13234_i64; // WIll be treated as ScVal::Object::I64
    let v2 = i2.into_env_val(&host);
    let obj: Object = v2.val.try_into().unwrap();
    assert!(obj.is_obj_type(ScObjectType::I64));
    assert_eq!(obj.get_handle(), 0);
    let k = i64::try_from(v2).unwrap();
    assert_eq!(i2, k);
}

#[test]
fn u32_as_seen_by_host() {
    let mut host = Host::default();
    let scval0 = ScVal::U32(12345);
    let val0 = host.to_host_val(&scval0).unwrap();
    assert!(val0.val.is::<u32>());
    assert!(val0.val.get_tag() == Tag::U32);
    let u = unsafe { <u32 as RawValConvertible>::unchecked_from_val(val0.val) };
    assert_eq!(u, 12345);
}

#[test]
fn i32_as_seen_by_host() {
    let mut host = Host::default();
    let scval0 = ScVal::I32(-12345);
    let val0 = host.to_host_val(&scval0).unwrap();
    assert!(val0.val.is::<i32>());
    assert!(val0.val.get_tag() == Tag::I32);
    let i = unsafe { <i32 as RawValConvertible>::unchecked_from_val(val0.val) };
    assert_eq!(i, -12345);
}

#[test]
fn vec_as_seen_by_host() -> Result<(), ()> {
    let mut host = Host::default();
    let scvec0: ScVec = ScVec(vec![ScVal::U32(1)].try_into()?);
    let scvec1: ScVec = ScVec(vec![ScVal::U32(1)].try_into()?);
    let scobj0: ScObject = ScObject::Vec(scvec0);
    let scobj1: ScObject = ScObject::Vec(scvec1);
    let scval0 = ScVal::Object(Some(Box::new(scobj0)));
    let scval1 = ScVal::Object(Some(Box::new(scobj1)));
    let val0 = host.to_host_val(&scval0).unwrap();
    let val1 = host.to_host_val(&scval1).unwrap();
    assert!(val0.val.is::<Object>());
    assert!(val1.val.is::<Object>());
    let obj0: Object = val0.val.try_into().unwrap();
    let obj1: Object = val1.val.try_into().unwrap();
    assert_eq!(obj0.get_handle(), 0);
    assert_eq!(obj1.get_handle(), 1);
    assert!(obj0.is_obj_type(ScObjectType::Vec));
    assert!(obj1.is_obj_type(ScObjectType::Vec));
    // Check that we got 2 distinct Vec objects
    assert_ne!(val0.val.get_payload(), val1.val.get_payload());
    // But also that they compare deep-equal.
    assert_eq!(val0, val1);
    Ok(())
}
