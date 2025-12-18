use crate::{Env, Host, HostError, NetworkLimits, Symbol, TryFromVal, TryIntoVal};
use soroban_test_wasms::{CONTRACT_STORAGE, LOADGEN};
use std::panic::{catch_unwind, AssertUnwindSafe};

// Helper to create a test host with limits enforcement enabled
fn test_host_with_limits() -> Host {
    let host = Host::test_host_with_recording_footprint();
    host.enable_network_limits_enforcement();
    host.enable_debug().unwrap();
    host.with_mut_ledger_info(|li| {
        li.sequence_number = 100;
        li.max_entry_ttl = 10000;
        li.min_persistent_entry_ttl = 1000;
        li.min_temp_entry_ttl = 16;
    })
    .unwrap();
    host
}

#[test]
fn test_write_entries_limit_not_exceeded() {
    let host = test_host_with_limits();
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);

    // Write 10 entries (well below the limit of 50)
    for i in 0..10 {
        let key = Symbol::try_from_small_str(&format!("key{}", i)).unwrap();
        let val: u64 = i;
        host.call(
            contract_id,
            Symbol::try_from_val(&host, &"put_persistent").unwrap(),
            test_vec![&host, key, val].into(),
        )
        .unwrap();
    }

    // Should succeed - we're under the limit
}

#[test]
fn test_write_entries_limit_exceeded() {
    // Use loadgen contract which can write multiple entries in a single call
    let host = Host::test_host_with_recording_footprint();
    host.enable_invocation_metering();
    host.enable_debug().unwrap();
    host.with_mut_ledger_info(|li| {
        li.sequence_number = 100;
        li.max_entry_ttl = 10000;
        li.min_persistent_entry_ttl = 1000;
        li.min_temp_entry_ttl = 16;
    })
    .unwrap();
    let contract_id = host.register_test_contract_wasm(LOADGEN);
    
    // Now enable limits enforcement for subsequent invocations
    host.enable_network_limits_enforcement();

    // Write 55 entries (exceeds the limit of 50)
    // do_work(guest_cycles, host_cycles, num_write_entries, size_kilo_bytes)
    // This should panic due to exceeded limits
    let result = catch_unwind(AssertUnwindSafe(|| {
        host.call(
            contract_id,
            Symbol::try_from_val(&host, &"do_work").unwrap(),
            test_vec![&host, 0_u64, 0_u64, 55_u32, 1_u32].into(),
        )
    }));

    // Should have panicked due to ExceededLimit
    assert!(result.is_err());
}

#[test]
fn test_read_entries_limit_not_exceeded() {
    let host = test_host_with_limits();
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);

    // Write some entries first
    for i in 0..10 {
        let key = Symbol::try_from_small_str(&format!("key{}", i)).unwrap();
        let val: u64 = i;
        host.call(
            contract_id,
            Symbol::try_from_val(&host, &"put_persistent").unwrap(),
            test_vec![&host, key, val].into(),
        )
        .unwrap();
    }

    // Read them back (should be well under the read limit of 100)
    for i in 0..10 {
        let key = Symbol::try_from_small_str(&format!("key{}", i)).unwrap();
        host.call(
            contract_id,
            Symbol::try_from_val(&host, &"has_persistent").unwrap(),
            test_vec![&host, key].into(),
        )
        .unwrap();
    }

    // Should succeed
}

#[test]
fn test_write_bytes_limit() {
    // Use loadgen contract which can write multiple entries with large sizes
    let host = Host::test_host_with_recording_footprint();
    host.enable_invocation_metering();
    host.enable_debug().unwrap();
    host.with_mut_ledger_info(|li| {
        li.sequence_number = 100;
        li.max_entry_ttl = 10000;
        li.min_persistent_entry_ttl = 1000;
        li.min_temp_entry_ttl = 16;
    })
    .unwrap();
    let contract_id = host.register_test_contract_wasm(LOADGEN);
    
    // Now enable limits enforcement
    host.enable_network_limits_enforcement();

    // Write 45 entries of 3KB each = 135KB total (exceeds 132096 byte limit)
    // do_work(guest_cycles, host_cycles, num_write_entries, size_kilo_bytes)
    // This should panic due to exceeded limits
    let result = catch_unwind(AssertUnwindSafe(|| {
        host.call(
            contract_id,
            Symbol::try_from_val(&host, &"do_work").unwrap(),
            test_vec![&host, 0_u64, 0_u64, 45_u32, 3_u32].into(),
        )
    }));

    // Should have panicked due to ExceededLimit
    assert!(result.is_err());
}

#[test]
fn test_custom_limits() {
    // Use loadgen contract
    let host = Host::test_host_with_recording_footprint();
    host.enable_invocation_metering();
    host.enable_debug().unwrap();
    host.with_mut_ledger_info(|li| {
        li.sequence_number = 100;
        li.max_entry_ttl = 10000;
        li.min_persistent_entry_ttl = 1000;
        li.min_temp_entry_ttl = 16;
    })
    .unwrap();
    let contract_id = host.register_test_contract_wasm(LOADGEN);
    
    // Set very low custom limits for testing
    let custom_limits = NetworkLimits {
        tx_max_disk_read_ledger_entries: 10,
        tx_max_write_ledger_entries: 5,
        tx_max_disk_read_bytes: 10000,
        tx_max_write_bytes: 5000,
        tx_max_contract_events_size_bytes: 1000,
    };
    host.enable_network_limits_enforcement_with_limits(custom_limits);

    // Try to write 6 entries (exceeds custom limit of 5)
    // do_work(guest_cycles, host_cycles, num_write_entries, size_kilo_bytes)
    // This should panic due to exceeded limits
    let result = catch_unwind(AssertUnwindSafe(|| {
        host.call(
            contract_id,
            Symbol::try_from_val(&host, &"do_work").unwrap(),
            test_vec![&host, 0_u64, 0_u64, 6_u32, 1_u32].into(),
        )
    }));

    // Should have panicked due to ExceededLimit
    assert!(result.is_err());
}

#[test]
fn test_limits_not_enforced_when_disabled() {
    // Create a host without limits enforcement
    let host = Host::test_host_with_recording_footprint();
    host.enable_invocation_metering();
    host.enable_debug().unwrap();
    host.with_mut_ledger_info(|li| {
        li.sequence_number = 100;
        li.max_entry_ttl = 10000;
        li.min_persistent_entry_ttl = 1000;
        li.min_temp_entry_ttl = 16;
    })
    .unwrap();

    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);

    // Write 60 entries (would exceed limit of 50 if enforced)
    let result = (|| -> Result<(), HostError> {
        for i in 0..60 {
            let key = Symbol::try_from_small_str(&format!("ky{:02}", i)).unwrap();
            let val: u64 = i;
            host.call(
                contract_id,
                Symbol::try_from_val(&host, &"put_persistent").unwrap(),
                test_vec![&host, key, val].into(),
            )?;
        }
        Ok(())
    })();

    // Should succeed because limits are not enforced
    result.unwrap();
}

#[test]
fn test_default_limits_match_stellar_core() {
    let limits = NetworkLimits::default();
    
    // Verify the default limits match pubnet_phase7.json values
    assert_eq!(limits.tx_max_disk_read_ledger_entries, 100);
    assert_eq!(limits.tx_max_write_ledger_entries, 50);
    assert_eq!(limits.tx_max_disk_read_bytes, 200_000);
    assert_eq!(limits.tx_max_write_bytes, 132_096);
    assert_eq!(limits.tx_max_contract_events_size_bytes, 16_384);
}
