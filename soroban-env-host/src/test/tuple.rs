use soroban_env_common::{IntoVal, TryIntoVal};

use crate::{CheckedEnv, Host, HostError, RawVal};

#[test]
fn tuple_conversions() -> Result<(), HostError> {
    let host = Host::default();

    let raw: RawVal = (1u32, 1i32).into_val(&host);

    let mut obj = host.vec_new(RawVal::VOID)?;
    obj = host.vec_push_back(obj, (1u32).into())?;
    obj = host.vec_push_back(obj, (1i32).into())?;

    assert_eq!(host.obj_cmp(raw, obj.to_raw())?, 0);

    let roundtrip: (u32, i32) = raw.try_into_val(&host)?;
    assert_eq!(roundtrip, (1u32, 1i32));

    Ok(())
}

#[test]
fn tuple_array_conversions() -> Result<(), HostError> {
    let host = Host::default();

    let raw: [RawVal; 0] = ().into_val(&host);
    let unit: () = raw.try_into_val(&host)?;
    assert_eq!(unit, ());

    let raw: [RawVal; 2] = (1u32, 1i32).into_val(&host);
    let roundtrip: (u32, i32) = raw.try_into_val(&host)?;
    assert_eq!(roundtrip, (1u32, 1i32));

    Ok(())
}
