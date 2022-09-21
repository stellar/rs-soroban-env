use soroban_env_host::Host;

use soroban_env_common::{xdr::ScObjectType, Env, Object, RawVal, TryIntoVal};

#[test]
fn u8_array() {
    let host = Host::default();

    let arr = [1u8, 2, 3];
    let val: RawVal = arr.try_into_val(&host).unwrap();
    let obj: Object = val.try_into().unwrap();
    assert!(obj.is_obj_type(ScObjectType::Bytes));
    assert_eq!(3u32, host.bytes_len(obj).try_into().unwrap());
    assert_eq!(1u32, host.bytes_get(obj, 0u32.into()).try_into().unwrap());
    assert_eq!(2u32, host.bytes_get(obj, 1u32.into()).try_into().unwrap());
    assert_eq!(3u32, host.bytes_get(obj, 2u32.into()).try_into().unwrap());

    let arr: [u8; 3] = val.try_into_val(&host).unwrap();
    assert_eq!(arr, [1, 2, 3]);
}

#[test]
fn u8_slice() {
    let host = Host::default();

    let slice: &[u8] = &[1u8, 2, 3];
    let val: RawVal = slice.try_into_val(&host).unwrap();
    let obj: Object = val.try_into().unwrap();
    assert!(obj.is_obj_type(ScObjectType::Bytes));
    assert_eq!(3u32, host.bytes_len(obj).try_into().unwrap());
    assert_eq!(1u32, host.bytes_get(obj, 0u32.into()).try_into().unwrap());
    assert_eq!(2u32, host.bytes_get(obj, 1u32.into()).try_into().unwrap());
    assert_eq!(3u32, host.bytes_get(obj, 2u32.into()).try_into().unwrap());

    let arr: [u8; 3] = val.try_into_val(&host).unwrap();
    assert_eq!(arr, [1, 2, 3]);
}
