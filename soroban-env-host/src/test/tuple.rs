use soroban_env_common::TryIntoVal;

use crate::{Env, Host, HostError, Val};

#[test]
fn tuple_conversions() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    let val: Val = (1u32, 1i32).try_into_val(&*host)?;

    let mut obj = host.vec_new()?;
    obj = host.vec_push_back(obj, (1u32).into())?;
    obj = host.vec_push_back(obj, (1i32).into())?;

    assert_eq!(host.obj_cmp(val, obj.into())?, 0);

    let roundtrip: (u32, i32) = val.try_into_val(&*host)?;
    assert_eq!(roundtrip, (1u32, 1i32));

    Ok(())
}

#[test]
fn tuple_array_conversions() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    let val: [Val; 0] = ().try_into_val(&*host)?;
    let _unit: () = val.try_into_val(&*host)?;

    let val: [Val; 2] = (1u32, 1i32).try_into_val(&*host)?;
    let roundtrip: (u32, i32) = val.try_into_val(&*host)?;
    assert_eq!(roundtrip, (1u32, 1i32));

    Ok(())
}
