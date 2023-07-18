use crate::{
    budget::Budget, host_object::HostVec, storage::Storage, Host, HostError, LedgerInfo,
    MeteredOrdMap,
};
use soroban_env_common::{Env, Symbol};
use soroban_test_wasms::{ADD_I32, COMPLEX};

use super::util::{generate_account_id, generate_bytes_array};

// The follow tests should be run with release profile, e.g.:
// cargo test --release --package soroban-env-host --lib -- test::metering_benchmark::run_complex --exact --nocapture

const LEDGER_INFO: LedgerInfo = LedgerInfo {
    protocol_version: 21,
    sequence_number: 1234,
    timestamp: 1234,
    network_id: [7; 32],
    base_reserve: 1,
    min_persistent_entry_expiration: 4096,
    min_temp_entry_expiration: 16,
    max_entry_expiration: 6312000,
};

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
        let (store, _, _, _) = host.try_finish().unwrap();
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
        let (store, _, _, _) = host.try_finish().unwrap();
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
