use crate::native_contract::testutils::HostVec;
use crate::{host_vec, Host};
use soroban_env_common::{AddressObject, Env, Symbol, TryFromVal, TryIntoVal};
use soroban_test_wasms::CONTRACT_STORAGE;

fn storage_fn_name(host: &Host, fn_name: &str, storage: &str) -> Symbol {
    Symbol::try_from_val(host, &format!("{}_{}", fn_name, storage).as_str()).unwrap()
}

fn test_storage(host: &Host, contract_id: AddressObject, storage: &str) {
    let key_1 = Symbol::try_from_small_str("key_1").unwrap();
    let key_2 = Symbol::try_from_val(host, &"this_is_key_2").unwrap();
    // Check that the key is not in the storage yet
    assert_eq!(
        bool::try_from_val(
            host,
            &host
                .call(
                    contract_id,
                    storage_fn_name(&host, "has", storage),
                    host_vec![host, key_1].into(),
                )
                .unwrap()
        )
        .unwrap(),
        false
    );
    // Put a key to storage and verify it's there
    host.call(
        contract_id,
        storage_fn_name(host, "put", storage),
        host_vec![host, key_1, 1234_u64].into(),
    )
    .unwrap();

    assert_eq!(
        bool::try_from_val(
            host,
            &host
                .call(
                    contract_id,
                    storage_fn_name(host, "has", storage),
                    host_vec![host, key_1].into(),
                )
                .unwrap()
        )
        .unwrap(),
        true
    );

    let max_entry_expiration: u32 = host.get_max_entry_expiration().unwrap().into();
    let max_bump = max_entry_expiration - 1;

    // Smoke test bump
    let bump_args = if storage == "instance" {
        host_vec![host, max_bump]
    } else {
        host_vec![host, key_1, max_bump]
    };

    host.call(
        contract_id,
        storage_fn_name(host, "bump", storage),
        bump_args.into(),
    )
    .unwrap();

    let bump_args_past_max = if storage == "instance" {
        host_vec![host, max_bump + 1]
    } else {
        host_vec![host, key_1, max_bump + 1]
    };

    assert!(host
        .call(
            contract_id,
            storage_fn_name(host, "bump", storage),
            bump_args_past_max.into(),
        )
        .is_err());

    // Put another key and verify it's there
    host.call(
        contract_id,
        storage_fn_name(host, "put", storage),
        host_vec![host, key_2, u64::MAX].into(),
    )
    .unwrap();
    assert_eq!(
        bool::try_from_val(
            host,
            &host
                .call(
                    contract_id,
                    storage_fn_name(host, "has", storage),
                    host_vec![
                        host,
                        // Use a new object to sanity-check that comparison
                        // happens based on value.
                        Symbol::try_from_val(host, &"this_is_key_2").unwrap(),
                    ]
                    .into(),
                )
                .unwrap()
        )
        .unwrap(),
        true
    );

    // Get values for both keys

    assert_eq!(
        u64::try_from_val(
            host,
            &host
                .call(
                    contract_id,
                    storage_fn_name(host, "get", storage),
                    host_vec![host, key_1].into(),
                )
                .unwrap()
        )
        .unwrap(),
        1234_u64
    );
    assert_eq!(
        u64::try_from_val(
            host,
            &host
                .call(
                    contract_id,
                    storage_fn_name(host, "get", storage),
                    host_vec![host, key_2].into(),
                )
                .unwrap()
        )
        .unwrap(),
        u64::MAX
    );

    // Update value for key 2 and check it
    host.call(
        contract_id,
        storage_fn_name(host, "put", storage),
        host_vec![host, key_2, 4321_u64].into(),
    )
    .unwrap();
    assert_eq!(
        u64::try_from_val(
            host,
            &host
                .call(
                    contract_id,
                    storage_fn_name(host, "get", storage),
                    host_vec![host, key_2].into(),
                )
                .unwrap()
        )
        .unwrap(),
        4321_u64
    );

    // Delete entry for key 1
    host.call(
        contract_id,
        storage_fn_name(host, "del", storage),
        host_vec![host, key_1].into(),
    )
    .unwrap();
    // Delete again - that's a no-op, but it shouldn't fail either.
    host.call(
        contract_id,
        storage_fn_name(host, "del", storage),
        host_vec![host, key_1].into(),
    )
    .unwrap();
    // Only the second key is now present
    assert_eq!(
        bool::try_from_val(
            host,
            &host
                .call(
                    contract_id,
                    storage_fn_name(host, "has", storage),
                    host_vec![host, key_1].into(),
                )
                .unwrap()
        )
        .unwrap(),
        false
    );
    assert_eq!(
        bool::try_from_val(
            host,
            &host
                .call(
                    contract_id,
                    storage_fn_name(host, "has", storage),
                    host_vec![host, key_2].into(),
                )
                .unwrap()
        )
        .unwrap(),
        true
    );
}

#[test]
fn test_persistent_storage() {
    let host = Host::test_host_with_recording_footprint();
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
    test_storage(&host, contract_id, "persistent");
}

#[test]
fn test_temp_storage() {
    let host = Host::test_host_with_recording_footprint();
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
    test_storage(&host, contract_id, "temporary");
}

#[test]
fn test_instance_storage() {
    let host = Host::test_host_with_recording_footprint();
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
    test_storage(&host, contract_id, "instance");
}

#[test]
fn test_storage_mix() {
    // This makes sure the keyspaces are not mixed between storage types.
    let host = Host::test_host_with_recording_footprint();
    host.with_budget(|b| {
        b.reset_unlimited().unwrap();
        Ok(())
    })
    .unwrap();
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
    test_storage(&host, contract_id, "persistent");
    test_storage(&host, contract_id, "temporary");
    test_storage(&host, contract_id, "instance");
}
