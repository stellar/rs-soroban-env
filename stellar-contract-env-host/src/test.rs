use crate::{
    xdr::{ScObject, ScObjectType, ScVal, ScVec},
    Host, IntoEnvVal, Object, RawVal, Tag,
};
use stellar_contract_env_common::{CheckedEnv, RawValConvertible};

use hex::FromHex;

#[cfg(feature = "vm")]
use crate::storage::{AccessType, Footprint, Storage};
#[cfg(feature = "vm")]
use crate::Vm;
#[cfg(feature = "vm")]
use crate::{
    xdr::{
        ContractDataEntry, Hash, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
        LedgerKeyContractData, ScStatic,
    },
    Symbol,
};
#[cfg(feature = "vm")]
use std::panic::{catch_unwind, AssertUnwindSafe};

/// numbers test
#[test]
fn u64_roundtrip() {
    let host = Host::default();
    let u: u64 = 38473_u64; // This will be treated as a ScVal::Object::U64
    let v = u.into_env_val(&host);
    let obj: Object = v.val.try_into().unwrap();
    assert!(obj.is_obj_type(ScObjectType::U64));
    assert_eq!(obj.get_handle(), 0);
    let j = u64::try_from(v).unwrap();
    assert_eq!(u, j);

    let u2: u64 = u64::MAX; // This will be treated as a ScVal::Object::U64
    let v2 = u2.into_env_val(&host);
    let obj: Object = v2.val.try_into().unwrap();
    assert!(obj.is_obj_type(ScObjectType::U64));
    assert_eq!(obj.get_handle(), 1);
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
    let host = Host::default();
    let scval0 = ScVal::U32(12345);
    let val0 = host.to_host_val(&scval0).unwrap();
    assert!(val0.val.is::<u32>());
    assert!(val0.val.get_tag() == Tag::U32);
    let u = unsafe { <u32 as RawValConvertible>::unchecked_from_val(val0.val) };
    assert_eq!(u, 12345);
}

#[test]
fn i32_as_seen_by_host() {
    let host = Host::default();
    let scval0 = ScVal::I32(-12345);
    let val0 = host.to_host_val(&scval0).unwrap();
    assert!(val0.val.is::<i32>());
    assert!(val0.val.get_tag() == Tag::I32);
    let i = unsafe { <i32 as RawValConvertible>::unchecked_from_val(val0.val) };
    assert_eq!(i, -12345);
}

/// Vec test
#[test]
fn vec_as_seen_by_host() -> Result<(), ()> {
    let host = Host::default();
    let scvec0: ScVec = ScVec(vec![ScVal::U32(1)].try_into()?);
    let scvec1: ScVec = ScVec(vec![ScVal::U32(1)].try_into()?);
    let scobj0: ScObject = ScObject::Vec(scvec0);
    let scobj1: ScObject = ScObject::Vec(scvec1);
    let scval0 = ScVal::Object(Some(scobj0));
    let scval1 = ScVal::Object(Some(scobj1));
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

#[test]
fn vec_front_and_back() -> Result<(), ()> {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    let front = unsafe {
        <i32 as RawValConvertible>::unchecked_from_val(host.vec_front(obj.to_object()).unwrap())
    };
    let back = unsafe {
        <i32 as RawValConvertible>::unchecked_from_val(host.vec_back(obj.to_object()).unwrap())
    };
    assert_eq!(front, 1);
    assert_eq!(back, 3);
    Ok(())
}

#[test]
#[should_panic(expected = "value does not exist")]
fn empty_vec_front() {
    let host = Host::default();
    let scvec: ScVec = vec![].try_into().unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_front(obj.to_object()).unwrap();
}

#[test]
#[should_panic(expected = "value does not exist")]
fn empty_vec_back() {
    let host = Host::default();
    let scvec: ScVec = vec![].try_into().unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_back(obj.to_object()).unwrap();
}

#[test]
fn vec_put_and_get() {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    let i: RawVal = 1_u32.into();
    let obj1 = host.vec_put(obj.to_object(), i, 9_u32.into()).unwrap();
    let rv = host.vec_get(obj1, i).unwrap();
    let v = unsafe { <u32 as RawValConvertible>::unchecked_from_val(rv) };
    assert_eq!(v, 9);
}

#[test]
fn vec_push_pop_and_len() {
    let host = Host::default();
    let scvec: ScVec = vec![].try_into().unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    let l = unsafe {
        <u32 as RawValConvertible>::unchecked_from_val(host.vec_len(obj.to_object()).unwrap())
    };
    assert_eq!(l, 0);
    let obj1 = host.vec_push(obj.to_object(), 1u32.into()).unwrap();
    let obj2 = host.vec_push(obj1, 2u32.into()).unwrap();
    let l = unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.vec_len(obj2).unwrap()) };
    assert_eq!(l, 2);
    let obj3 = host.vec_pop(obj2).unwrap();
    let l = unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.vec_len(obj3).unwrap()) };
    assert_eq!(l, 1);
    let obj4 = host.vec_pop(obj3).unwrap();
    let l = unsafe { <u32 as RawValConvertible>::unchecked_from_val(host.vec_len(obj4).unwrap()) };
    assert_eq!(l, 0);
}

#[test]
#[should_panic(expected = "value does not exist")]
fn vec_pop_empty_vec() {
    let host = Host::default();
    let scvec: ScVec = vec![].try_into().unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_pop(obj.to_object()).unwrap();
}

#[test]
#[should_panic(expected = "index out of bound")]
fn vec_get_out_of_bound() {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_get(obj.to_object(), 3_u32.into()).unwrap();
}

#[test]
#[should_panic(expected = "i must be u32")]
fn vec_get_wrong_index_type() {
    let host = Host::default();
    let scvec: ScVec = vec![].try_into().unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_get(obj.to_object(), (-1_i32).into()).unwrap();
}

#[test]
fn vec_del_and_cmp() {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let obj = host.to_host_obj(&ScObject::Vec(scvec)).unwrap();
    let obj1 = host.vec_del(obj.to_object(), 1u32.into()).unwrap();
    let scvec_ref: ScVec = vec![ScVal::U32(1), ScVal::U32(3)].try_into().unwrap();
    let obj_ref = host.to_host_obj(&ScObject::Vec(scvec_ref)).unwrap();
    assert_eq!(host.obj_cmp(obj1.into(), obj_ref.into()).unwrap(), 0);
}

#[test]
#[should_panic(expected = "index out of bound")]
fn vec_del_out_of_bound() {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_del(obj.to_object(), 3_u32.into()).unwrap();
}

#[test]
#[should_panic(expected = "i must be u32")]
fn vec_del_wrong_index_type() {
    let host = Host::default();
    let scvec: ScVec = vec![].try_into().unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_del(obj.to_object(), (-1_i32).into()).unwrap();
}

#[test]
fn vec_slice_and_cmp() {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let obj = host.to_host_obj(&ScObject::Vec(scvec)).unwrap();
    let obj1 = host
        .vec_slice(obj.to_object(), 1u32.into(), 2u32.into())
        .unwrap();
    let scvec_ref: ScVec = vec![ScVal::U32(2), ScVal::U32(3)].try_into().unwrap();
    let obj_ref = host.to_host_obj(&ScObject::Vec(scvec_ref)).unwrap();
    assert_eq!(host.obj_cmp(obj1.into(), obj_ref.into()).unwrap(), 0);

    let obj2 = host
        .vec_slice(obj.to_object(), 0u32.into(), 3u32.into())
        .unwrap();
    assert_ne!(obj2.as_ref().get_payload(), obj.as_raw().get_payload());
    assert_eq!(host.obj_cmp(obj2.into(), obj.into()).unwrap(), 0);
}

#[test]
#[should_panic(expected = "u32 overflow")]
fn vec_slice_index_overflow() {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_slice(obj.to_object(), u32::MAX.into(), 1_u32.into())
        .unwrap();
}

#[test]
#[should_panic(expected = "index out of bound")]
fn vec_slice_out_of_bound() {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_slice(obj.to_object(), 0_u32.into(), 4_u32.into())
        .unwrap();
}

#[test]
#[should_panic(expected = "i must be u32")]
fn vec_take_wrong_index_type() {
    let host = Host::default();
    let scvec: ScVec = vec![].try_into().unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_slice(obj.to_object(), (-1_i32).into(), 1_u32.into())
        .unwrap();
}

#[test]
#[should_panic(expected = "l must be u32")]
fn vec_take_wrong_len_type() {
    let host = Host::default();
    let scvec: ScVec = vec![].try_into().unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_slice(obj.to_object(), 1_u32.into(), (-1_i32).into())
        .unwrap();
}

#[test]
fn vec_insert_and_cmp() {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(2)].try_into().unwrap();
    let obj = host.to_host_obj(&ScObject::Vec(scvec)).unwrap();
    let obj1 = host
        .vec_insert(obj.to_object(), 0u32.into(), 1u32.into())
        .unwrap();
    let scvec_ref: ScVec = vec![ScVal::U32(1), ScVal::U32(2)].try_into().unwrap();
    let obj_ref = host.to_host_obj(&ScObject::Vec(scvec_ref)).unwrap();
    assert_eq!(host.obj_cmp(obj1.into(), obj_ref.into()).unwrap(), 0);

    let obj2 = host.vec_insert(obj1, 2u32.into(), 3u32.into()).unwrap();
    let scvec_ref: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let obj_ref = host.to_host_obj(&ScObject::Vec(scvec_ref)).unwrap();
    assert_eq!(host.obj_cmp(obj2.into(), obj_ref.into()).unwrap(), 0);
}

#[test]
#[should_panic(expected = "index out of bound")]
fn vec_insert_out_of_bound() {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_insert(obj.to_object(), 4_u32.into(), 9u32.into())
        .unwrap();
}

#[test]
#[should_panic(expected = "i must be u32")]
fn vec_insert_wrong_index_type() {
    let host = Host::default();
    let scvec: ScVec = vec![].try_into().unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_insert(obj.to_object(), (-1_i32).into(), 9u32.into())
        .unwrap();
}

#[test]
fn vec_append() {
    let host = Host::default();
    let scvec0: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let obj0 = host.to_host_obj(&ScObject::Vec(scvec0)).unwrap();
    let scvec1: ScVec = vec![ScVal::U32(4), ScVal::U32(5), ScVal::U32(6)]
        .try_into()
        .unwrap();
    let obj1 = host.to_host_obj(&ScObject::Vec(scvec1)).unwrap();
    let obj2 = host.vec_append(*obj0.as_ref(), *obj1.as_ref()).unwrap();
    let scvec_ref: ScVec = vec![
        ScVal::U32(1),
        ScVal::U32(2),
        ScVal::U32(3),
        ScVal::U32(4),
        ScVal::U32(5),
        ScVal::U32(6),
    ]
    .try_into()
    .unwrap();
    let obj_ref = host.to_host_obj(&ScObject::Vec(scvec_ref)).unwrap();
    assert_eq!(host.obj_cmp(obj2.into(), obj_ref.into()).unwrap(), 0);
}

#[test]
fn vec_append_empty() {
    let host = Host::default();
    let scvec0: ScVec = vec![].try_into().unwrap();
    let obj0 = host.to_host_obj(&ScObject::Vec(scvec0)).unwrap();
    let obj1 = host.vec_append(*obj0.as_ref(), *obj0.as_ref()).unwrap();
    assert_ne!(obj0.as_raw().get_payload(), obj1.as_ref().get_payload());
    assert_eq!(host.obj_cmp(obj0.into(), obj1.into()).unwrap(), 0);
}

/// crypto tests
#[test]
fn sha256_test() {
    let host = Host::default();
    let obj0 = host
        .to_host_obj(&ScObject::Binary(vec![1].try_into().unwrap()))
        .unwrap();
    let hash_obj = host.compute_hash_sha256(obj0.to_object()).unwrap();

    let v = host.from_host_val(hash_obj.to_raw()).unwrap();
    let bin = match v {
        ScVal::Object(Some(scobj)) => match scobj {
            ScObject::Binary(bin) => bin,
            _ => panic!("Wrong type"),
        },
        _ => panic!("Wrong type"),
    };

    /*
    We took the sha256 of [1], which is 4bf5122f344554c53bde2ebb8cd2b7e3d1600ad631c385a5d7cce23c7785459a
    The exp array contains the decimal representation of each hex value
    */
    let exp: Vec<u8> = vec![
        75, 245, 18, 47, 52, 69, 84, 197, 59, 222, 46, 187, 140, 210, 183, 227, 209, 96, 10, 214,
        49, 195, 133, 165, 215, 204, 226, 60, 119, 133, 69, 154,
    ];
    assert_eq!(bin.as_vec().clone(), exp);
}

#[test]
fn ed25519_verify_test() {
    let host = Host::default();

    // From https://datatracker.ietf.org/doc/html/rfc8032#section-7.1

    // First verify successfully
    let public_key: &[u8] = b"3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c";
    let message: &[u8] = b"72";
    let signature: &[u8] = b"92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00";

    let pub_bytes: Vec<u8> = FromHex::from_hex(public_key).unwrap();
    let msg_bytes: Vec<u8> = FromHex::from_hex(message).unwrap();
    let sig_bytes: Vec<u8> = FromHex::from_hex(signature).unwrap();

    let obj_pub = host
        .to_host_obj(&ScObject::Binary(pub_bytes.try_into().unwrap()))
        .unwrap();
    let obj_msg = host
        .to_host_obj(&ScObject::Binary(msg_bytes.try_into().unwrap()))
        .unwrap();
    let obj_sig = host
        .to_host_obj(&ScObject::Binary(sig_bytes.try_into().unwrap()))
        .unwrap();

    let res = host.verify_sig_ed25519(
        obj_msg.to_object(),
        obj_pub.to_object(),
        obj_sig.to_object(),
    );

    match res {
        Ok(_) => (),
        _ => panic!("verification test failed"),
    };

    // Now verify with wrong message
    let message2: &[u8] = b"73";
    let msg_bytes2: Vec<u8> = FromHex::from_hex(message2).unwrap();
    let obj_msg2 = host
        .to_host_obj(&ScObject::Binary(msg_bytes2.try_into().unwrap()))
        .unwrap();

    let res_failed = host.verify_sig_ed25519(
        obj_msg2.to_object(),
        obj_pub.to_object(),
        obj_sig.to_object(),
    );

    match res_failed {
        Ok(_) => panic!("verification test failed"),
        _ => (),
    };
}

/// VM test
/**
 This is an example WASM from the SDK that unpacks two SCV_I32 arguments, adds
 them with an overflow check, and re-packs them as an SCV_I32 if successful.

 To regenerate, check out the SDK, install a nightly toolchain with
 the rust-src component (to enable the 'tiny' build) using the following:

  $ rustup component add rust-src --toolchain nightly

 then do:

  $ make tiny
  $ xxd -i target/wasm32-unknown-unknown/release/example_add_i32.wasm
*/
#[cfg(feature = "vm")]
#[test]
fn invoke_single_contract_function() -> Result<(), ()> {
    let host = Host::default();
    let code: [u8; 163] = [
        0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x07, 0x01, 0x60, 0x02, 0x7e, 0x7e,
        0x01, 0x7e, 0x03, 0x02, 0x01, 0x00, 0x05, 0x03, 0x01, 0x00, 0x10, 0x06, 0x11, 0x02, 0x7f,
        0x00, 0x41, 0x80, 0x80, 0xc0, 0x00, 0x0b, 0x7f, 0x00, 0x41, 0x80, 0x80, 0xc0, 0x00, 0x0b,
        0x07, 0x2b, 0x04, 0x06, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x02, 0x00, 0x03, 0x61, 0x64,
        0x64, 0x00, 0x00, 0x0a, 0x5f, 0x5f, 0x64, 0x61, 0x74, 0x61, 0x5f, 0x65, 0x6e, 0x64, 0x03,
        0x00, 0x0b, 0x5f, 0x5f, 0x68, 0x65, 0x61, 0x70, 0x5f, 0x62, 0x61, 0x73, 0x65, 0x03, 0x01,
        0x0a, 0x47, 0x01, 0x45, 0x01, 0x02, 0x7f, 0x02, 0x40, 0x20, 0x00, 0x42, 0x0f, 0x83, 0x42,
        0x03, 0x52, 0x20, 0x01, 0x42, 0x0f, 0x83, 0x42, 0x03, 0x52, 0x72, 0x45, 0x04, 0x40, 0x20,
        0x01, 0x42, 0x04, 0x88, 0xa7, 0x22, 0x02, 0x41, 0x00, 0x48, 0x20, 0x02, 0x20, 0x00, 0x42,
        0x04, 0x88, 0xa7, 0x22, 0x03, 0x6a, 0x22, 0x02, 0x20, 0x03, 0x48, 0x73, 0x45, 0x0d, 0x01,
        0x0b, 0x00, 0x0b, 0x20, 0x02, 0xad, 0x42, 0x04, 0x86, 0x42, 0x03, 0x84, 0x0b,
    ];
    let id: Hash = [0; 32].into();
    let vm = Vm::new(&host, id, &code).unwrap();
    let a = 4i32;
    let b = 7i32;
    let c = 0x7fffffff_i32;
    let scvec0: ScVec = ScVec(vec![ScVal::I32(a), ScVal::I32(b)].try_into()?);
    let res = vm.invoke_function(&host, "add", &scvec0).unwrap();
    match res {
        ScVal::I32(v) => assert_eq!(v, a + b),
        _ => panic!("Wrong result type"),
    }
    // overflow
    let scvec0: ScVec = ScVec(vec![ScVal::I32(a), ScVal::I32(c)].try_into()?);
    let res = catch_unwind(AssertUnwindSafe(|| {
        vm.invoke_function(&host, "add", &scvec0).unwrap();
    }));
    assert!(res.is_err());
    Ok(())
}

#[cfg(feature = "vm")]
#[test]
fn contract_invoke_another_contract() -> Result<(), ()> {
    use im_rc::OrdMap;

    let contract_id: Hash = [0; 32].into();
    let key = ScVal::Static(ScStatic::LedgerKeyContractCodeWasm);
    let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
        contract_id: contract_id.clone(),
        key: key.clone(),
    });
    let code: [u8; 163] = [
        0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x07, 0x01, 0x60, 0x02, 0x7e, 0x7e,
        0x01, 0x7e, 0x03, 0x02, 0x01, 0x00, 0x05, 0x03, 0x01, 0x00, 0x10, 0x06, 0x11, 0x02, 0x7f,
        0x00, 0x41, 0x80, 0x80, 0xc0, 0x00, 0x0b, 0x7f, 0x00, 0x41, 0x80, 0x80, 0xc0, 0x00, 0x0b,
        0x07, 0x2b, 0x04, 0x06, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x02, 0x00, 0x03, 0x61, 0x64,
        0x64, 0x00, 0x00, 0x0a, 0x5f, 0x5f, 0x64, 0x61, 0x74, 0x61, 0x5f, 0x65, 0x6e, 0x64, 0x03,
        0x00, 0x0b, 0x5f, 0x5f, 0x68, 0x65, 0x61, 0x70, 0x5f, 0x62, 0x61, 0x73, 0x65, 0x03, 0x01,
        0x0a, 0x47, 0x01, 0x45, 0x01, 0x02, 0x7f, 0x02, 0x40, 0x20, 0x00, 0x42, 0x0f, 0x83, 0x42,
        0x03, 0x52, 0x20, 0x01, 0x42, 0x0f, 0x83, 0x42, 0x03, 0x52, 0x72, 0x45, 0x04, 0x40, 0x20,
        0x01, 0x42, 0x04, 0x88, 0xa7, 0x22, 0x02, 0x41, 0x00, 0x48, 0x20, 0x02, 0x20, 0x00, 0x42,
        0x04, 0x88, 0xa7, 0x22, 0x03, 0x6a, 0x22, 0x02, 0x20, 0x03, 0x48, 0x73, 0x45, 0x0d, 0x01,
        0x0b, 0x00, 0x0b, 0x20, 0x02, 0xad, 0x42, 0x04, 0x86, 0x42, 0x03, 0x84, 0x0b,
    ];
    let scob = ScObject::Binary(code.try_into()?);
    let val = ScVal::Object(Some(scob));
    let le = LedgerEntry {
        last_modified_ledger_seq: 0,
        data: LedgerEntryData::ContractData(ContractDataEntry {
            contract_id,
            key,
            val,
        }),
        ext: LedgerEntryExt::V0,
    };
    let map = OrdMap::unit(storage_key.clone(), Some(le));
    let mut footprint = Footprint::default();
    footprint.record_access(&storage_key, AccessType::ReadOnly);

    // initialize storage and host
    let storage = Storage::with_enforcing_footprint_and_map(footprint, map);
    let host = Host::with_storage(storage);
    // create a dummy contract obj as the caller
    let scobj = ScObject::Binary([0; 32].try_into()?);
    let obj = host.to_host_obj(&scobj).unwrap();
    // prepare arguments
    let sym = Symbol::from_str("add");
    let scvec0: ScVec = vec![ScVal::I32(1), ScVal::I32(2)].try_into().unwrap();
    let args = host.to_host_obj(&ScObject::Vec(scvec0)).unwrap();

    let res = host.call(obj.to_object(), sym.into(), args.into()).unwrap();
    assert!(res.is::<i32>());
    assert!(res.get_tag() == Tag::I32);
    let i: i32 = res.try_into()?;
    assert_eq!(i, 3);
    Ok(())
}
