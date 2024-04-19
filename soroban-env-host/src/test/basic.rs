use soroban_env_common::{Tag, TryFromVal, TryIntoVal, Val};

use crate::{host::HostError, xdr::ScVal, Host, Object};

/// numbers test
#[test]
fn u64_roundtrip() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let u: u64 = 38473_u64; // This will be treated as a U64Small
    let v: Val = u.try_into_val(&*host)?;
    assert_eq!(v.get_tag(), Tag::U64Small);
    let j = u64::try_from_val(&*host, &v)?;
    assert_eq!(u, j);

    let u2: u64 = u64::MAX; // This will be treated as a U64Object
    let v2: Val = u2.try_into_val(&*host)?;
    assert_eq!(v2.get_tag(), Tag::U64Object);
    let obj: Object = v2.try_into()?;
    assert_eq!(obj.get_handle(), 1);
    let k = u64::try_from_val(&*host, &v2)?;
    assert_eq!(u2, k);
    Ok(())
}

#[test]
fn i64_roundtrip() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let i: i64 = 12345_i64; // Will be treated as I64Small
    let v: Val = i.try_into_val(&*host)?;
    assert_eq!(v.get_tag(), Tag::I64Small);
    let j = i64::try_from_val(&*host, &v)?;
    assert_eq!(i, j);

    let i2: i64 = i64::MAX; // Will be treated as I64Object
    let v2: Val = i2.try_into_val(&*host)?;
    assert_eq!(v2.get_tag(), Tag::I64Object);
    let obj: Object = v2.try_into()?;
    assert_eq!(obj.get_handle(), 1);
    let k = i64::try_from_val(&*host, &v2)?;
    assert_eq!(i2, k);
    Ok(())
}

#[test]
fn u32_as_seen_by_host() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let scval0 = ScVal::U32(12345);
    let val0 = host.to_host_val(&scval0)?;
    assert_eq!(val0.get_tag(), Tag::U32Val);
    let u: u32 = val0.try_into()?;
    assert_eq!(u, 12345);
    Ok(())
}

#[test]
fn i32_as_seen_by_host() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let scval0 = ScVal::I32(-12345);
    let val0 = host.to_host_val(&scval0)?;
    assert_eq!(val0.get_tag(), Tag::I32Val);
    let i: i32 = val0.try_into()?;
    assert_eq!(i, -12345);
    Ok(())
}

#[test]
fn tuple_roundtrip() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let t0: (u32, i32) = (5, -4);
    let ev: Val = t0.try_into_val(&*host)?;
    let t0_back: (u32, i32) = <(u32, i32)>::try_from_val(&*host, &ev)?;
    assert_eq!(t0, t0_back);
    Ok(())
}

#[test]
fn f32_does_not_work() -> Result<(), HostError> {
    use soroban_env_common::xdr::Hash;
    let host = observe_host!(Host::test_host());
    let hash = Hash::from([0; 32]);
    assert!(HostError::result_matches_err(
        crate::vm::Vm::new(&host, hash, soroban_test_wasms::ADD_F32),
        (
            crate::xdr::ScErrorType::WasmVm,
            crate::xdr::ScErrorCode::InvalidAction
        )
    ));
    Ok(())
}
