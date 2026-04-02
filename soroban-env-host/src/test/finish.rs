use crate::Host;

#[test]
fn can_finish_on_one_reference() {
    let host1 = Host::test_host_with_recording_footprint();
    assert!(host1.try_finish().is_ok());
}

#[test]
fn cant_finish_on_multiple_reference() {
    let host1 = Host::test_host_with_recording_footprint();
    let host2 = host1.clone();
    // Can't finish when multiple references exist
    assert!(host1.try_finish().is_err());
    // After host1 was consumed by try_finish (even though it failed),
    // host2 is the only remaining reference and can finish.
    assert!(host2.try_finish().is_ok());
}

#[test]
fn can_finish_on_reduced_to_one_reference_count() {
    let host1 = Host::test_host_with_recording_footprint();
    {
        let host2 = host1.clone();
        assert!(host2.try_finish().is_err());
    }
    assert!(host1.try_finish().is_ok());
}
