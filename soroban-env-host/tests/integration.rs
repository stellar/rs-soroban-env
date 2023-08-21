use expect_test::expect;
use soroban_env_common::EnvBase;
use soroban_env_host::{Compare, DiagnosticLevel, Env, Host, HostError, Tag, Val};

#[test]
fn vec_as_seen_by_user() -> Result<(), HostError> {
    let host = Host::default();
    let int1 = host.obj_from_i64(5)?;

    let vec1a = host.vec_new()?;
    let vec1b = host.vec_push_back(vec1a, *int1.as_ref())?;

    assert_ne!(vec1a.as_val().get_payload(), vec1b.as_val().get_payload());

    let vec2a = host.vec_new()?;
    let vec2b = host.vec_push_back(vec2a, *int1.as_ref())?;

    assert_ne!(vec2a.as_val().get_payload(), vec2b.as_val().get_payload());
    assert_ne!(vec1b.as_val().get_payload(), vec2b.as_val().get_payload());
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
fn map_host_fn() -> Result<(), HostError> {
    let host = Host::default();
    let m = host.map_new()?;
    assert_eq!(m.as_val().get_tag(), Tag::MapObject);
    Ok(())
}

#[test]
fn debug_log() {
    let host = Host::default();
    host.set_diagnostic_level(DiagnosticLevel::Debug).unwrap();
    // Call a debug-log helper.
    host.log_from_slice("can't convert value", &[Val::from_i32(1).to_val()])
        .unwrap();

    // Fish out the last debug event and check that it is
    // correct, and formats as expected.
    let events = host.get_events().unwrap().0;
    let last_event = &events.last().unwrap();
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[r#"[Diagnostic Event] topics:[log], data:["can't convert value", 1]"#]];
    let actual = format!("{}", last_event);
    expected.assert_eq(&actual);
}
