use soroban_env_host::Host;

use soroban_env_common::{xdr::ScObjectType, Env, IntoVal, Object, RawVal, TryIntoVal};

#[test]
fn array() {
    let host = Host::default();

    let val: RawVal = [1i32, 2, 3].into_val(&host);
    let obj: Object = val.try_into().unwrap();
    assert!(obj.is_obj_type(ScObjectType::Vec));
    assert_eq!(3u32, host.vec_len(obj).try_into().unwrap());
    assert_eq!(1i32, host.vec_get(obj, 0u32.into()).try_into().unwrap());
    assert_eq!(2i32, host.vec_get(obj, 1u32.into()).try_into().unwrap());
    assert_eq!(3i32, host.vec_get(obj, 2u32.into()).try_into().unwrap());

    let arr: [i32; 3] = val.try_into_val(&host).unwrap();
    assert_eq!(arr, [1, 2, 3]);
}

// #[test]
// fn array_u8() {
//     let host = Host::default();

//     let val: RawVal = [1u8, 2, 3].into_val(&host);
//     let obj: Object = val.try_into().unwrap();
//     assert!(obj.is_obj_type(ScObjectType::Bytes));
//     assert_eq!(3u32, host.binary_len(obj).try_into().unwrap());
//     assert_eq!(1u32, host.binary_get(obj, 0u32.into()).try_into().unwrap());
//     assert_eq!(2u32, host.binary_get(obj, 1u32.into()).try_into().unwrap());
//     assert_eq!(3u32, host.binary_get(obj, 2u32.into()).try_into().unwrap());

//     let arr: [u8; 3] = val.try_into_val(&host).unwrap();
//     assert_eq!(arr, [1, 2, 3]);
// }
