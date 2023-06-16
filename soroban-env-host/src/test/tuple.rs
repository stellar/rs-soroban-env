use soroban_env_common::TryIntoVal;

use crate::{Env, Host, HostError, Val};

#[test]
fn tuple_conversions() -> Result<(), HostError> {
    let host = Host::default();

    let raw: Val = (1u32, 1i32).try_into_val(&host)?;

    let mut obj = host.vec_new(Val::VOID.into())?;
    obj = host.vec_push_back(obj, (1u32).into())?;
    obj = host.vec_push_back(obj, (1i32).into())?;

    assert_eq!(host.obj_cmp(raw, obj.into())?, 0);

    let roundtrip: (u32, i32) = raw.try_into_val(&host)?;
    assert_eq!(roundtrip, (1u32, 1i32));

    Ok(())
}

#[test]
fn tuple_array_conversions() -> Result<(), HostError> {
    let host = Host::default();

    let raw: [Val; 0] = ().try_into_val(&host)?;
    let _unit: () = raw.try_into_val(&host)?;

    let raw: [Val; 2] = (1u32, 1i32).try_into_val(&host)?;
    let roundtrip: (u32, i32) = raw.try_into_val(&host)?;
    assert_eq!(roundtrip, (1u32, 1i32));

    Ok(())
}
