use stellar_contract_env_host::{
    events::HostEvent, xdr::ScObjectType, Env, EnvBase, Host, Object, RawVal,
};

#[test]
fn vec_as_seen_by_user() -> Result<(), ()> {
    let host = Host::default();
    let int1 = host.obj_from_i64(5).in_env(&host);

    let vec1a = host.vec_new(RawVal::from_void()).in_env(&host);
    let vec1b = host.vec_push(vec1a.val, *int1.as_ref()).in_env(&host);

    assert_ne!(vec1a.as_raw().get_payload(), vec1b.as_raw().get_payload());

    let vec2a = host.vec_new(RawVal::from_void()).in_env(&host);
    let vec2b = host.vec_push(vec2a.val, *int1.as_ref()).in_env(&host);

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

#[test]
fn debug_fmt() {
    let host = Host::default();

    // Call a "native formatting-style" debug helper.
    host.log_static_fmt_val_static_str(
        "can't convert {} to {}",
        RawVal::from_i32(1),
        std::any::type_name::<Vec<u8>>(),
    );

    // Fish out the last debug event and check that it is
    // correct, and formats as expected.
    let events = host.get_events();
    match events.0.last() {
        Some(HostEvent::Debug(de)) => {
            assert_eq!(
                format!("{}", de),
                "can't convert I32(1) to alloc::vec::Vec<u8>"
            )
        }
        _ => {
            panic!("missing debug event")
        }
    }
}
