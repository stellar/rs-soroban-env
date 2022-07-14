use stellar_contract_env_host::{xdr::ScObjectType, Env, Host, Object, RawVal};

#[test]
fn vec_as_seen_by_user() -> Result<(), ()> {
    let host = Host::default();
    let int1 = host.obj_from_i64(5).in_env(&host);

    let vec1a = host.vec_new(RawVal::from_void()).in_env(&host);
    let vec1b = host.vec_push(*vec1a.as_ref(), *int1.as_ref()).in_env(&host);

    assert_ne!(vec1a.as_raw().get_payload(), vec1b.as_raw().get_payload());

    let vec2a = host.vec_new(RawVal::from_void()).in_env(&host);
    let vec2b = host.vec_push(*vec2a.as_ref(), *int1.as_ref()).in_env(&host);

    assert_ne!(vec2a.as_raw().get_payload(), vec2b.as_raw().get_payload());
    assert_ne!(vec1b.as_raw().get_payload(), vec2b.as_raw().get_payload());
    assert_eq!(vec1a, vec2a);
    assert_eq!(vec1b, vec2b);
    Ok(())
}

#[test]
fn vec_host_fn() {
    let host = Host::default();
    let m = host.map_new();
    assert!(Object::val_is_obj_type(m.into(), ScObjectType::Map));
}
