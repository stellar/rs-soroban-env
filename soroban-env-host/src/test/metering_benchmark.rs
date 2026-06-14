use crate::{
    budget::Budget,
    host_object::HostVec,
    storage::{Footprint, Storage},
    Host, HostError, LedgerInfo, MeteredOrdMap,
};
use soroban_env_common::{Env, StorageType, Symbol, TryFromVal, TryIntoVal, Val};
use soroban_test_wasms::{ADD_I32, COMPLEX, CONTRACT_STORAGE};
use std::time::Instant;

use crate::testutils::{generate_account_id, generate_bytes_array};

// The follow tests enables resource (cpu and mem) trackers, their main purpose is to evaluate
// metering accuracy by comparing modeled resource usage from the budget vs actual resource usage.
// They should be run with release profile, i.e. `cargo test --release` to get a fair comparison.
//
// These tests should be disabled by default, due to two reasons
// 1. it needs to be run with the release profile whereas normal tests run with the test profile.
// 2. they cannot be run in parallel with multiple threads, due to contention of the `global_tracker`.
//
// Run with the following command:
// RUST_TEST_THREADS=1  cargo test --release --package soroban-env-host --lib --features testutils -- test::metering_benchmark  --nocapture --ignored

const LEDGER_INFO: LedgerInfo = LedgerInfo {
    protocol_version: crate::meta::INTERFACE_VERSION.protocol,
    sequence_number: 1234,
    timestamp: 1234,
    network_id: [7; 32],
    base_reserve: 1,
    min_persistent_entry_ttl: 4096,
    min_temp_entry_ttl: 16,
    max_entry_ttl: 6312000,
};

#[ignore]
#[test]
fn run_add_i32() -> Result<(), HostError> {
    #[cfg(feature = "tracy")]
    tracy_client::Client::start();

    let _test_span = tracy_span!("run_add_i32 test");

    let host = Host::test_host_with_recording_footprint();
    let account_id = generate_account_id(&host);
    let salt = generate_bytes_array(&host);
    let a = 4i32;
    let b = 7i32;

    // Run 1: record footprint, emulating "preflight".
    let foot = {
        let _run_span = tracy_span!("add_i32 run 1: recording footprint");
        host.set_ledger_info(LEDGER_INFO.clone())?;
        let contract_id_obj = host.register_test_contract_wasm_from_source_account(
            ADD_I32,
            account_id.clone(),
            salt,
        )?;
        host.call(
            contract_id_obj,
            Symbol::try_from_small_str("add")?,
            host.test_vec_obj(&[a, b])?,
        )?;
        let (store, _) = host.try_finish().unwrap();
        store.footprint
    };
    // Run 2: enforce preflight footprint
    {
        let _run_span = tracy_span!("add_i32 run 2: enforcing footprint");
        let store = Storage::with_enforcing_footprint_and_map(
            Footprint::default(),
            MeteredOrdMap::default(),
        );
        let host = Host::with_storage_and_budget(store, Budget::default());
        host.set_ledger_info(LEDGER_INFO)?;
        host.setup_storage_footprint(foot)?;
        let contract_id_obj =
            host.register_test_contract_wasm_from_source_account(ADD_I32, account_id, salt)?;
        host.measured_call(
            contract_id_obj,
            Symbol::try_from_small_str("add")?,
            host.test_vec_obj(&[a, b])?,
        )?;
    }
    // Give tracy a little time to extract data.
    #[cfg(feature = "tracy")]
    std::thread::sleep(std::time::Duration::from_secs(2));
    Ok(())
}

#[ignore]
#[test]
fn run_complex() -> Result<(), HostError> {
    #[cfg(feature = "tracy")]
    tracy_client::Client::start();

    let _test_span = tracy_span!("run_complex test");
    let host = Host::test_host_with_recording_footprint();
    let account_id = generate_account_id(&host);
    let salt = generate_bytes_array(&host);

    // Run 1: record footprint, emulating "preflight".
    let foot = {
        let _run_span = tracy_span!("complex run 1: recording footprint");
        host.set_ledger_info(LEDGER_INFO.clone())?;
        let contract_id_obj = host.register_test_contract_wasm_from_source_account(
            COMPLEX,
            account_id.clone(),
            salt,
        )?;
        host.call(
            contract_id_obj,
            Symbol::try_from_small_str("go")?,
            host.add_host_object(HostVec::new())?,
        )?;
        let (store, _) = host.try_finish().unwrap();
        store.footprint
    };

    // Run 2: enforce preflight footprint, with empty map -- contract should only write.
    {
        let _run_span = tracy_span!("complex run 2: enforcing footprint");
        let store = Storage::with_enforcing_footprint_and_map(
            Footprint::default(),
            MeteredOrdMap::default(),
        );
        let host = Host::with_storage_and_budget(store, Budget::default());
        host.set_ledger_info(LEDGER_INFO)?;
        host.setup_storage_footprint(foot)?;
        let contract_id_obj =
            host.register_test_contract_wasm_from_source_account(COMPLEX, account_id, salt)?;
        host.measured_call(
            contract_id_obj,
            Symbol::try_from_small_str("go")?,
            host.add_host_object(HostVec::new())?,
        )?;
    }
    // Give tracy a little time to extract data.
    #[cfg(feature = "tracy")]
    std::thread::sleep(std::time::Duration::from_secs(2));
    Ok(())
}

fn measure_storage_read(
    host: &Host,
    label: &str,
    iterations: u64,
    mut f: impl FnMut() -> Result<Val, HostError>,
) -> Result<(), HostError> {
    host.with_budget(|budget| {
        budget.reset_unlimited()?;
        Ok(())
    })?;
    let start = Instant::now();
    for _ in 0..iterations {
        f()?;
    }
    let elapsed = start.elapsed();
    let budget = host.budget_cloned();
    println!(
        "{label}: iterations={iterations} cpu_insns={} mem_bytes={} wall_nsecs={}",
        budget.get_cpu_insns_consumed()?,
        budget.get_mem_bytes_consumed()?,
        elapsed.as_nanos()
    );
    Ok(())
}

fn storage_symbol(host: &Host, name: &str) -> Symbol {
    Symbol::try_from_val(host, &name).unwrap()
}

fn instance_size_key(host: &Host, i: usize) -> Symbol {
    Symbol::try_from_val(host, &format!("ikey{i}").as_str()).unwrap()
}

// This is a lightweight benchmark harness for comparing the new host external
// read path against a target contract view function that returns the same data.
//
// Run with:
// RUST_TEST_THREADS=1 cargo test --release -p soroban-env-host --lib --features testutils,next \
//   test::metering_benchmark::compare_external_storage_read_to_contract_view_call \
//   -- --ignored --nocapture
#[ignore]
#[test]
fn compare_external_storage_read_to_contract_view_call() -> Result<(), HostError> {
    const ITERATIONS: u64 = 100;

    let host = Host::test_host_with_recording_footprint();
    host.set_ledger_info(LEDGER_INFO)?;
    host.with_budget(|b| {
        b.reset_unlimited()?;
        Ok(())
    })?;
    let contract = host.register_test_contract_wasm(CONTRACT_STORAGE);

    let persistent_key = Symbol::try_from_small_str("persist")?;
    let temporary_key = Symbol::try_from_small_str("temp")?;
    let instance_key = Symbol::try_from_small_str("instance")?;

    host.call(
        contract,
        storage_symbol(&host, "put_persistent"),
        test_vec![&host, persistent_key, 7_u64].into(),
    )?;
    host.call(
        contract,
        storage_symbol(&host, "put_temporary"),
        test_vec![&host, temporary_key, 7_u64].into(),
    )?;
    host.call(
        contract,
        storage_symbol(&host, "put_instance"),
        test_vec![&host, instance_key, 7_u64].into(),
    )?;

    // Warm VM/module caches and storage footprints before measuring the steady path.
    host.call(
        contract,
        storage_symbol(&host, "get_persistent"),
        test_vec![&host, persistent_key].into(),
    )?;
    host.get_external_contract_data(contract, persistent_key.to_val(), StorageType::Persistent)?;
    host.call(
        contract,
        storage_symbol(&host, "get_temporary"),
        test_vec![&host, temporary_key].into(),
    )?;
    host.get_external_contract_data(contract, temporary_key.to_val(), StorageType::Temporary)?;
    host.call(
        contract,
        storage_symbol(&host, "get_instance"),
        test_vec![&host, instance_key].into(),
    )?;
    host.get_external_contract_data(contract, instance_key.to_val(), StorageType::Instance)?;

    measure_storage_read(&host, "contract_view_persistent", ITERATIONS, || {
        host.call(
            contract,
            storage_symbol(&host, "get_persistent"),
            test_vec![&host, persistent_key].into(),
        )
    })?;
    measure_storage_read(&host, "external_read_persistent", ITERATIONS, || {
        host.get_external_contract_data(contract, persistent_key.to_val(), StorageType::Persistent)
    })?;
    measure_storage_read(&host, "contract_view_temporary", ITERATIONS, || {
        host.call(
            contract,
            storage_symbol(&host, "get_temporary"),
            test_vec![&host, temporary_key].into(),
        )
    })?;
    measure_storage_read(&host, "external_read_temporary", ITERATIONS, || {
        host.get_external_contract_data(contract, temporary_key.to_val(), StorageType::Temporary)
    })?;
    measure_storage_read(&host, "contract_view_instance", ITERATIONS, || {
        host.call(
            contract,
            storage_symbol(&host, "get_instance"),
            test_vec![&host, instance_key].into(),
        )
    })?;
    measure_storage_read(&host, "external_read_instance", ITERATIONS, || {
        host.get_external_contract_data(contract, instance_key.to_val(), StorageType::Instance)
    })?;

    let mut current_instance_entries = 0;
    for target_instance_entries in [1, 8, 32] {
        for i in current_instance_entries..target_instance_entries {
            host.call(
                contract,
                storage_symbol(&host, "put_instance"),
                test_vec![&host, instance_size_key(&host, i), i as u64].into(),
            )?;
        }
        current_instance_entries = target_instance_entries;
        let read_key = instance_size_key(&host, 0);
        host.get_external_contract_data(contract, read_key.to_val(), StorageType::Instance)?;
        let label = format!("external_read_instance_{target_instance_entries}_entries");
        measure_storage_read(&host, &label, ITERATIONS, || {
            host.get_external_contract_data(contract, read_key.to_val(), StorageType::Instance)
        })?;
    }

    Ok(())
}
