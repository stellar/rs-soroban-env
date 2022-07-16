use crate::{
    host::HostError,
    xdr::{ScObjectType, ScVal},
    Host, IntoEnvVal, Object, RawValConvertible, Tag,
};

/// numbers test
#[test]
fn u64_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let u: u64 = 38473_u64; // This will be treated as a ScVal::Object::U64
    let v = u.into_env_val(&host);
    let obj: Object = v.val.try_into()?;
    assert!(obj.is_obj_type(ScObjectType::U64));
    assert_eq!(obj.get_handle(), 0);
    let j = u64::try_from(v)?;
    assert_eq!(u, j);

    let u2: u64 = u64::MAX; // This will be treated as a ScVal::Object::U64
    let v2 = u2.into_env_val(&host);
    let obj: Object = v2.val.try_into()?;
    assert!(obj.is_obj_type(ScObjectType::U64));
    assert_eq!(obj.get_handle(), 1);
    let k = u64::try_from(v2)?;
    assert_eq!(u2, k);
    Ok(())
}

#[test]
fn i64_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let i: i64 = 12345_i64; // Will be treated as ScVal::I64
    let v = i.into_env_val(&host);
    let j = i64::try_from(v)?;
    assert_eq!(i, j);

    let i2: i64 = -13234_i64; // WIll be treated as ScVal::Object::I64
    let v2 = i2.into_env_val(&host);
    let obj: Object = v2.val.try_into()?;
    assert!(obj.is_obj_type(ScObjectType::I64));
    assert_eq!(obj.get_handle(), 0);
    let k = i64::try_from(v2)?;
    assert_eq!(i2, k);
    Ok(())
}

#[test]
fn u32_as_seen_by_host() -> Result<(), HostError> {
    let host = Host::default();
    let scval0 = ScVal::U32(12345);
    let val0 = host.to_host_val(&scval0)?;
    assert!(val0.val.is::<u32>());
    assert!(val0.val.get_tag() == Tag::U32);
    let u = unsafe { <u32 as RawValConvertible>::unchecked_from_val(val0.val) };
    assert_eq!(u, 12345);
    Ok(())
}

#[test]
fn i32_as_seen_by_host() -> Result<(), HostError> {
    let host = Host::default();
    let scval0 = ScVal::I32(-12345);
    let val0 = host.to_host_val(&scval0)?;
    assert!(val0.val.is::<i32>());
    assert!(val0.val.get_tag() == Tag::I32);
    let i = unsafe { <i32 as RawValConvertible>::unchecked_from_val(val0.val) };
    assert_eq!(i, -12345);
    Ok(())
}

#[test]
fn tuple_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let t0: (u32, i32) = (5, -4);
    let ev = t0.into_env_val(&host);
    let t0_back: (u32, i32) = ev.try_into()?;
    assert_eq!(t0, t0_back);
    Ok(())
}
