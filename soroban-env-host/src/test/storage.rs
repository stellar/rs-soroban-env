use crate::native_contract::testutils::HostVec;
use crate::{host_vec, Host};
use soroban_env_common::{Env, Symbol, TryFromVal, TryIntoVal};
use soroban_test_wasms::CONTRACT_STORAGE;

#[test]
fn test_persistent_storage() {
    let host = Host::test_host_with_recording_footprint();
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
    let key_1 = Symbol::try_from_small_str("key_1").unwrap();
    let key_2 = Symbol::try_from_val(&host, &"this_is_key_2").unwrap();
    // Check that the key is not in the storage yet
    assert_eq!(
        bool::try_from_val(
            &host,
            &host
                .call(
                    contract_id,
                    Symbol::try_from_val(&host, &"has_recreatable").unwrap(),
                    host_vec![&host, key_1].into(),
                )
                .unwrap()
        )
        .unwrap(),
        false
    );
    // Put a key to storage and verify it's there
    host.call(
        contract_id,
        Symbol::try_from_val(&host, &"put_recreatable").unwrap(),
        host_vec![&host, key_1, 1234_u64, ()].into(),
    )
    .unwrap();

    assert_eq!(
        bool::try_from_val(
            &host,
            &host
                .call(
                    contract_id,
                    Symbol::try_from_val(&host, &"has_recreatable").unwrap(),
                    host_vec![&host, key_1].into(),
                )
                .unwrap()
        )
        .unwrap(),
        true
    );

    // Put anothrer key and verify it's there
    host.call(
        contract_id,
        Symbol::try_from_val(&host, &"put_recreatable").unwrap(),
        host_vec![&host, key_2, u64::MAX, ()].into(),
    )
    .unwrap();
    assert_eq!(
        bool::try_from_val(
            &host,
            &host
                .call(
                    contract_id,
                    Symbol::try_from_val(&host, &"has_recreatable").unwrap(),
                    host_vec![
                        &host,
                        // Use a new object to sanity-check that comparison
                        // happens based on value.
                        Symbol::try_from_val(&host, &"this_is_key_2").unwrap(),
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
            &host,
            &host
                .call(
                    contract_id,
                    Symbol::try_from_val(&host, &"get_recreatable").unwrap(),
                    host_vec![&host, key_1].into(),
                )
                .unwrap()
        )
        .unwrap(),
        1234_u64
    );
    assert_eq!(
        u64::try_from_val(
            &host,
            &host
                .call(
                    contract_id,
                    Symbol::try_from_val(&host, &"get_recreatable").unwrap(),
                    host_vec![&host, key_2].into(),
                )
                .unwrap()
        )
        .unwrap(),
        u64::MAX
    );

    // Update value for key 2 and check it
    host.call(
        contract_id,
        Symbol::try_from_val(&host, &"put_recreatable").unwrap(),
        host_vec![&host, key_2, 4321_u64, ()].into(),
    )
    .unwrap();
    assert_eq!(
        u64::try_from_val(
            &host,
            &host
                .call(
                    contract_id,
                    Symbol::try_from_val(&host, &"get_recreatable").unwrap(),
                    host_vec![&host, key_2].into(),
                )
                .unwrap()
        )
        .unwrap(),
        4321_u64
    );

    // Delete entry for key 1
    host.call(
        contract_id,
        Symbol::try_from_val(&host, &"del_recreatable").unwrap(),
        host_vec![&host, key_1].into(),
    )
    .unwrap();
    // Only the second key is now present
    assert_eq!(
        bool::try_from_val(
            &host,
            &host
                .call(
                    contract_id,
                    Symbol::try_from_val(&host, &"has_recreatable").unwrap(),
                    host_vec![&host, key_1].into(),
                )
                .unwrap()
        )
        .unwrap(),
        false
    );
    assert_eq!(
        bool::try_from_val(
            &host,
            &host
                .call(
                    contract_id,
                    Symbol::try_from_val(&host, &"has_recreatable").unwrap(),
                    host_vec![&host, key_2].into(),
                )
                .unwrap()
        )
        .unwrap(),
        true
    );
}
