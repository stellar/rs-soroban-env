use soroban_env_host::{events::Event, Compare, Env, EnvBase, Host, HostError, MapObject, RawVal};

#[test]
fn vec_as_seen_by_user() -> Result<(), HostError> {
    let host = Host::default();
    let int1 = host.obj_from_i64(5)?;

    let vec1a = host.vec_new(RawVal::from_void().into())?;
    let vec1b = host.vec_push_back(vec1a, *int1.as_ref())?;

    assert_ne!(vec1a.as_raw().get_payload(), vec1b.as_raw().get_payload());

    let vec2a = host.vec_new(RawVal::from_void().into())?;
    let vec2b = host.vec_push_back(vec2a, *int1.as_ref())?;

    assert_ne!(vec2a.as_raw().get_payload(), vec2b.as_raw().get_payload());
    assert_ne!(vec1b.as_raw().get_payload(), vec2b.as_raw().get_payload());
    assert_eq!(
        host.compare(&vec1a, &vec2a).unwrap(),
        core::cmp::Ordering::Equal
    );
    assert_eq!(
        host.compare(&vec1b, &vec2b).unwrap(),
        core::cmp::Ordering::Equal
    );
    Ok(())
}

#[test]
fn vec_host_fn() -> Result<(), HostError> {
    let host = Host::default();
    let m = host.map_new()?;
    assert!(m.as_raw().is::<MapObject>());
    Ok(())
}

#[test]
fn debug_fmt() {
    let host = Host::default();

    // Call a "native formatting-style" debug helper.
    host.log_static_fmt_val_static_str(
        "can't convert {} to {}",
        RawVal::from_i32(1).to_raw(),
        std::any::type_name::<Vec<u8>>(),
    )
    .unwrap();

    // Fish out the last debug event and check that it is
    // correct, and formats as expected.
    let events = host.get_events().unwrap().0;
    match &events.last().unwrap().event {
        Event::Debug(de) => {
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
