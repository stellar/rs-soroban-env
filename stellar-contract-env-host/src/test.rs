use crate::{
    xdr::{ScObject, ScObjectType, ScVal, ScVec},
    Host, IntoEnvVal, Object,
};

#[test]
fn i64_roundtrip() {
    let host = Host::default();
    let i = 12345_i64;
    let v = i.into_env_val(&host);
    let j = i64::try_from(v).unwrap();
    assert_eq!(i, j);
}

#[test]
fn vec_as_seen_by_host() -> Result<(), ()> {
    let mut host = Host::default();
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
