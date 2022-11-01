use soroban_env_host::{events::HostEvent, xdr::ScObjectType, Env, EnvBase, Host, Object, RawVal};

#[test]
fn vec_as_seen_by_user() -> Result<(), ()> {
    let host = Host::default();
    let int1 = RawVal::from_i64(5);

    let vec1a = host.vec_new(RawVal::from_void()).in_env(&host);
    let vec1b = host.vec_push_back(vec1a.val, int1.clone()).in_env(&host);

    assert_ne!(vec1a, vec1b);

    let vec2a = host.vec_new(RawVal::from_void()).in_env(&host);
    let vec2b = host.vec_push_back(vec2a.val, int1).in_env(&host);

    assert_ne!(vec2a, vec2b);
    assert!(!vec1b.to_raw().shallow_eq(&vec2b.to_raw()));
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
    )
    .unwrap();

    // Fish out the last debug event and check that it is
    // correct, and formats as expected.
    let events = host.get_events().unwrap();
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
