use crate::{
    xdr::{ScObject, ScObjectType, ScVal, ScVec},
    EnvValConvertible, Host, Object, OrAbort,
};

#[test]
fn i64_roundtrip() {
    let host = Host::default();
    let i = 12345_i64;
    let v = i.into_env_val(&host);
    let j = i64::try_from_env_val(&v).unwrap();
    assert_eq!(i, j);
}

#[test]
fn vec_as_seen_by_host() -> Result<(), ()> {
    let mut host = Host::default();
    let scvec0: ScVec = ScVec(vec![ScVal::ScvU32(1)].try_into()?);
    let scvec1: ScVec = ScVec(vec![ScVal::ScvU32(1)].try_into()?);
    let scobj0: ScObject = ScObject::ScoVec(scvec0);
    let scobj1: ScObject = ScObject::ScoVec(scvec1);
    let scval0 = ScVal::ScvObject(Some(Box::new(scobj0)));
    let scval1 = ScVal::ScvObject(Some(Box::new(scobj1)));
    let val0 = host.to_host_val(&scval0).or_abort();
    let val1 = host.to_host_val(&scval1).or_abort();
    assert!(val0.val.is::<Object>());
    assert!(val1.val.is::<Object>());
    let obj0: Object = val0.val.try_into().or_abort();
    let obj1: Object = val1.val.try_into().or_abort();
    assert_eq!(obj0.get_handle(), 0);
    assert_eq!(obj1.get_handle(), 1);
    assert!(obj0.is_obj_type(ScObjectType::ScoVec));
    assert!(obj1.is_obj_type(ScObjectType::ScoVec));
    // Check that we got 2 distinct Vec objects
    assert_ne!(val0.val.get_payload(), val1.val.get_payload());
    // But also that they compare deep-equal.
    assert_eq!(val0, val1);
    Ok(())
}
