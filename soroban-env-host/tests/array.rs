use soroban_env_host::{BytesObject, Host, HostError};

use soroban_env_common::{Env, TryIntoVal, Val};

#[test]
fn u8_array() -> Result<(), HostError> {
    let host = Host::default();

    let arr = [1u8, 2, 3];
    let val: Val = arr.try_into_val(&host).unwrap();
    let obj: BytesObject = val.try_into().unwrap();
    assert_eq!(3u32, host.bytes_len(obj)?.try_into()?);
    assert_eq!(1u32, host.bytes_get(obj, 0u32.into())?.try_into()?);
    assert_eq!(2u32, host.bytes_get(obj, 1u32.into())?.try_into()?);
    assert_eq!(3u32, host.bytes_get(obj, 2u32.into())?.try_into()?);

    let arr: [u8; 3] = val.try_into_val(&host)?;
    assert_eq!(arr, [1, 2, 3]);
    Ok(())
}

#[test]
fn u8_slice() -> Result<(), HostError> {
    let host = Host::default();

    let slice: &[u8] = &[1u8, 2, 3];
    let val: Val = slice.try_into_val(&host)?;
    let obj: BytesObject = val.try_into()?;
    assert_eq!(3u32, host.bytes_len(obj)?.try_into()?);
    assert_eq!(1u32, host.bytes_get(obj, 0u32.into())?.try_into()?);
    assert_eq!(2u32, host.bytes_get(obj, 1u32.into())?.try_into()?);
    assert_eq!(3u32, host.bytes_get(obj, 2u32.into())?.try_into()?);

    let arr: [u8; 3] = val.try_into_val(&host)?;
    assert_eq!(arr, [1, 2, 3]);
    Ok(())
}
