use crate::{
    budget::Budget,
    host_object::HostVec,
    storage::{Footprint, Storage},
    Host, HostError, LedgerInfo, MeteredOrdMap,
};
use soroban_env_common::{Env, Symbol};
use soroban_test_wasms::{ADD_I32, COMPLEX};

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
    protocol_version: crate::meta::get_ledger_protocol_version(crate::meta::INTERFACE_VERSION),
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
