use crate::{
    budget::Budget,
    host_object::HostVec,
    storage::{Footprint, Storage},
    testutils::{generate_account_id, generate_bytes_array},
    Host, HostError, MeteredOrdMap,
};
use soroban_env_common::{Env, Symbol};
use soroban_test_wasms::COMPLEX;

use super::observe::ObservedHost;

#[test]
fn run_complex() -> Result<(), HostError> {
    let info = crate::LedgerInfo {
        protocol_version: Host::current_test_protocol(),
        sequence_number: 1234,
        timestamp: 1234,
        network_id: [7; 32],
        base_reserve: 1,
        min_persistent_entry_ttl: 4096,
        min_temp_entry_ttl: 16,
        max_entry_ttl: 6312000,
    };

    let host = ObservedHost::new(
        "soroban_env_host::test::complex::run_complex_1",
        Host::test_host_with_recording_footprint(),
    );
    let account_id = generate_account_id(&host);
    let salt = generate_bytes_array(&host);

    // Run 1: record footprint, emulating "preflight".
    let foot = {
        host.set_ledger_info(info.clone())?;
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
        let realhost: Host = (*host).clone();
        drop(host);
        let (store, _) = realhost.try_finish().unwrap();
        store.footprint
    };

    // Run 2: enforce preflight footprint, with empty map -- contract should only write.
    {
        let store = Storage::with_enforcing_footprint_and_map(
            Footprint::default(),
            MeteredOrdMap::default(),
        );
        let host = Host::with_storage_and_budget(store, Budget::default());
        host.set_ledger_info(info)?;
        let host = ObservedHost::new("soroban_env_host::test::complex::run_complex_2", host);
        host.setup_storage_footprint(foot)?;

        let contract_id_obj =
            host.register_test_contract_wasm_from_source_account(COMPLEX, account_id, salt)?;
        host.call(
            contract_id_obj,
            Symbol::try_from_small_str("go")?,
            host.add_host_object(HostVec::new())?,
        )?;
    }
    Ok(())
}
