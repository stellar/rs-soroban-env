use soroban_env_common::{
    I64Object, I64Small, RawVal, TryFromVal, TryIntoVal, U64Object, U64Small,
};

use crate::{host::HostError, xdr::ScVal, Host, Object, RawValConvertible, Tag};

/// numbers test
#[test]
fn u64_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let u: u64 = 38473_u64; // This will be treated as a U64Small
    let v: RawVal = u.try_into_val(&host)?;
    assert!(v.is::<U64Small>());
    let j = u64::try_from_val(&host, &v)?;
    assert_eq!(u, j);

    let u2: u64 = u64::MAX; // This will be treated as a U64Object
    let v2: RawVal = u2.try_into_val(&host)?;
    assert!(v2.is::<U64Object>());
    let obj: Object = v2.try_into()?;
    assert_eq!(obj.get_handle(), 0);
    let k = u64::try_from_val(&host, &v2)?;
    assert_eq!(u2, k);
    Ok(())
}

#[test]
fn i64_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let i: i64 = 12345_i64; // Will be treated as I64Small
    let v: RawVal = i.try_into_val(&host)?;
    assert!(v.is::<I64Small>());
    let j = i64::try_from_val(&host, &v)?;
    assert_eq!(i, j);

    let i2: i64 = i64::MAX; // Will be treated as I64Object
    let v2: RawVal = i2.try_into_val(&host)?;
    assert!(v2.is::<I64Object>());
    let obj: Object = v2.try_into()?;
    assert_eq!(obj.get_handle(), 0);
    let k = i64::try_from_val(&host, &v2)?;
    assert_eq!(i2, k);
    Ok(())
}

#[test]
fn u32_as_seen_by_host() -> Result<(), HostError> {
    let host = Host::default();
    let scval0 = ScVal::U32(12345);
    let val0 = host.to_host_val(&scval0)?;
    assert!(val0.is::<u32>());
    assert!(val0.get_tag() == Tag::U32Val);
    let u = unsafe { <u32 as RawValConvertible>::unchecked_from_val(val0) };
    assert_eq!(u, 12345);
    Ok(())
}

#[test]
fn i32_as_seen_by_host() -> Result<(), HostError> {
    let host = Host::default();
    let scval0 = ScVal::I32(-12345);
    let val0 = host.to_host_val(&scval0)?;
    assert!(val0.is::<i32>());
    assert!(val0.get_tag() == Tag::I32Val);
    let i = unsafe { <i32 as RawValConvertible>::unchecked_from_val(val0) };
    assert_eq!(i, -12345);
    Ok(())
}

#[test]
fn tuple_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let t0: (u32, i32) = (5, -4);
    let ev: RawVal = t0.try_into_val(&host)?;
    let t0_back: (u32, i32) = <(u32, i32)>::try_from_val(&host, &ev)?;
    assert_eq!(t0, t0_back);
    Ok(())
}

#[test]
fn f32_does_not_work() -> Result<(), HostError> {
    use soroban_env_common::xdr::Hash;
    let host = Host::default();
    let hash = Hash::from([0; 32]);
    assert!(crate::vm::Vm::new(&host, hash, soroban_test_wasms::ADD_F32).is_err());
    Ok(())
}
