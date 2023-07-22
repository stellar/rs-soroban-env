use crate::{
    budget::Budget, host_object::HostVec, storage::Storage, Host, HostError, LedgerInfo,
    MeteredOrdMap,
};
use soroban_env_common::{Env, Symbol};
use soroban_test_wasms::{ADD_I32, COMPLEX};

use super::util::{generate_account_id, generate_bytes_array};

// The follow tests enables resource (cpu and mem) trackers, their main purpose is to evaluate
// metering accuracy by comparing modeled resource usage from the budget vs actual resource usage.
// They should be run with release profile, i.e. `cargo test --release` to get a fair comparison.
//
// These tests should be disabled by default, due to two reasons
// 1. it needs to be run with the release profile whereas normal tests run with the test profile.
// 2. they cannot be run in parallel with multiple threads, due to contention of the `global_tracker`.
//
// Run with the following command:
// RUST_TEST_THREADS=1  cargo test --release --package soroban-env-host --lib -- test::metering_benchmark  --nocapture --ignored

const LEDGER_INFO: LedgerInfo = LedgerInfo {
    protocol_version: 21,
    sequence_number: 1234,
    timestamp: 1234,
    network_id: [7; 32],
    base_reserve: 1,
    min_persistent_entry_expiration: 4096,
    min_temp_entry_expiration: 16,
    max_entry_expiration: 6312000,
    autobump_ledgers: 0,
};

#[ignore]
#[test]
fn run_add_i32() -> Result<(), HostError> {
    let account_id = generate_account_id();
    let salt = generate_bytes_array();
    let a = 4i32;
    let b = 7i32;

    // Run 1: record footprint, emulating "preflight".
    let foot = {
        let host = Host::test_host_with_recording_footprint();
        host.set_ledger_info(LEDGER_INFO.clone())?;
        let contract_id_obj = host.register_test_contract_wasm_from_source_account(
            ADD_I32,
            account_id.clone(),
            salt.clone(),
        );
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
        let store = Storage::with_enforcing_footprint_and_map(foot, MeteredOrdMap::default());
        let host = Host::with_storage_and_budget(store, Budget::default());
        host.set_ledger_info(LEDGER_INFO)?;
        let contract_id_obj =
            host.register_test_contract_wasm_from_source_account(ADD_I32, account_id, salt);
        host.measured_call(
            contract_id_obj,
            Symbol::try_from_small_str("add")?,
            host.test_vec_obj(&[a, b])?,
        )?;
    }
    Ok(())
}

#[ignore]
#[test]
fn run_complex() -> Result<(), HostError> {
    let account_id = generate_account_id();
    let salt = generate_bytes_array();

    // Run 1: record footprint, emulating "preflight".
    let foot = {
        let host = Host::test_host_with_recording_footprint();
        host.set_ledger_info(LEDGER_INFO.clone())?;
        let contract_id_obj = host.register_test_contract_wasm_from_source_account(
            COMPLEX,
            account_id.clone(),
            salt.clone(),
        );
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
        let store = Storage::with_enforcing_footprint_and_map(foot, MeteredOrdMap::default());
        let host = Host::with_storage_and_budget(store, Budget::default());
        host.set_ledger_info(LEDGER_INFO)?;
        let contract_id_obj =
            host.register_test_contract_wasm_from_source_account(COMPLEX, account_id, salt);
        host.measured_call(
            contract_id_obj,
            Symbol::try_from_small_str("go")?,
            host.add_host_object(HostVec::new())?,
        )?;
    }
    Ok(())
}
