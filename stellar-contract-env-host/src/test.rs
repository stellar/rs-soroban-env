use stellar_contract_env_common::{CheckedEnv, EnvVal, RawValConvertible};

use crate::{
    xdr::{ScObject, ScObjectType, ScVal, ScVec},
    Host, IntoEnvVal, Object, RawVal, Tag,
};

#[test]
fn u64_roundtrip() {
    let host = Host::default();
    let u: u64 = 38473_u64; // This will be treated as a ScVal::Object::U64
    let v: EnvVal<_, RawVal> = u.into_env_val(&host);
    let obj: Object = v.val.try_into().unwrap();
    assert!(obj.is_obj_type(ScObjectType::U64));
    assert_eq!(obj.get_handle(), 0);
    let j = u64::try_from(v).unwrap();
    assert_eq!(u, j);

    let u2: u64 = u64::MAX; // This will be treated as a ScVal::Object::U64
    let v2: EnvVal<_, RawVal> = u2.into_env_val(&host);
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

/// Vec
#[test]
fn vec_as_seen_by_host() -> Result<(), ()> {
    let host = Host::default();
    let scvec0: ScVec = ScVec(vec![ScVal::U32(1)].try_into()?);
    let scvec1: ScVec = ScVec(vec![ScVal::U32(1)].try_into()?);
    let scobj0: ScObject = ScObject::Vec(scvec0);
    let scobj1: ScObject = ScObject::Vec(scvec1);
    let scval0 = ScVal::Object(Some(Box::new(scobj0)));
    let scval1 = ScVal::Object(Some(Box::new(scobj1)));
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
fn vec_take_and_cmp() {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let obj = host.to_host_obj(&ScObject::Vec(scvec)).unwrap();
    let obj1 = host.vec_take(obj.to_object(), 2u32.into()).unwrap();
    let scvec_ref: ScVec = vec![ScVal::U32(1), ScVal::U32(2)].try_into().unwrap();
    let obj_ref = host.to_host_obj(&ScObject::Vec(scvec_ref)).unwrap();
    assert_eq!(host.obj_cmp(obj1.into(), obj_ref.into()).unwrap(), 0);

    let obj2 = host.vec_take(obj.to_object(), 3u32.into()).unwrap();
    assert_ne!(obj2.as_ref().get_payload(), obj.as_raw().get_payload());
    assert_eq!(host.obj_cmp(obj2.into(), obj.into()).unwrap(), 0);
}

#[test]
#[should_panic(expected = "index out of bound")]
fn vec_take_out_of_bound() {
    let host = Host::default();
    let scvec: ScVec = vec![ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
        .try_into()
        .unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_del(obj.to_object(), 4_u32.into()).unwrap();
}

#[test]
#[should_panic(expected = "n must be u32")]
fn vec_take_wrong_index_type() {
    let host = Host::default();
    let scvec: ScVec = vec![].try_into().unwrap();
    let scobj = ScObject::Vec(scvec);
    let obj = host.to_host_obj(&scobj).unwrap();
    host.vec_take(obj.to_object(), (-1_i32).into()).unwrap();
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
