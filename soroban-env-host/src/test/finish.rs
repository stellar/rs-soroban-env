use crate::Host;

#[test]
fn can_finish_on_one_reference() {
    let host1 = Host::test_host_with_recording_footprint();
    assert!(host1.can_finish());
    assert!(host1.try_finish().is_ok());
}

#[test]
fn cant_finish_on_multiple_reference() {
    let host1 = Host::test_host_with_recording_footprint();
    let host2 = host1.clone();
    assert!(!host1.can_finish());
    assert!(!host2.can_finish());
    assert!(host1.try_finish().is_err());
    _ = host2;
}

#[test]
fn can_finish_on_reduced_to_one_reference_count() {
    let host1 = Host::test_host_with_recording_footprint();
    {
        let host2 = host1.clone();
        assert!(!host1.can_finish());
        assert!(!host2.can_finish());
        _ = host1;
    }
    assert!(host1.can_finish());
    assert!(host1.try_finish().is_ok());
}
