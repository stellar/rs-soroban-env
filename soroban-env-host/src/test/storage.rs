use std::rc::Rc;

use crate::budget::{AsBudget, Budget};
use crate::host_object::MuxedScAddress;
use crate::storage::{AccessType, Footprint, Storage};
use crate::xdr::{
    ContractDataDurability, ContractId, LedgerKey, LedgerKeyContractData, MuxedEd25519Account,
    ScAddress, ScErrorCode, ScErrorType, ScVal, Uint256,
};
use crate::{Host, HostError, MeteredOrdMap};
use soroban_env_common::{AddressObject, Env, MuxedAddressObject, Symbol, TryFromVal, TryIntoVal};
use soroban_test_wasms::{CONTRACT_STORAGE, CONTRACT_STORAGE_WITH_VALS, INVOKE_CONTRACT};

#[test]
fn footprint_record_access() -> Result<(), HostError> {
    let budget = Budget::default();
    budget.reset_unlimited()?;
    let mut fp = Footprint::default();
    // record when key not exist
    let key = Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
        contract: ScAddress::Contract(ContractId([0; 32].into())),
        key: ScVal::I32(0),
        durability: ContractDataDurability::Persistent,
    }));
    fp.record_access(&key, AccessType::ReadOnly, &budget)?;
    assert_eq!(fp.0.contains_key::<LedgerKey>(&key, &budget)?, true);
    assert_eq!(
        fp.0.get::<LedgerKey>(&key, &budget)?,
        Some(&AccessType::ReadOnly)
    );
    // record and change access
    fp.record_access(&key, AccessType::ReadWrite, &budget)?;
    assert_eq!(
        fp.0.get::<LedgerKey>(&key, &budget)?,
        Some(&AccessType::ReadWrite)
    );
    fp.record_access(&key, AccessType::ReadOnly, &budget)?;
    assert_eq!(
        fp.0.get::<LedgerKey>(&key, &budget)?,
        Some(&AccessType::ReadWrite)
    );
    Ok(())
}

#[test]
fn footprint_enforce_access() -> Result<(), HostError> {
    let budget = Budget::default();
    let key = Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
        contract: ScAddress::Contract(ContractId([0; 32].into())),
        key: ScVal::I32(0),
        durability: ContractDataDurability::Persistent,
    }));

    // Key not in footprint. Only difference is type_
    let key2 = Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
        contract: ScAddress::Contract(ContractId([0; 32].into())),
        key: ScVal::I32(0),
        durability: ContractDataDurability::Temporary,
    }));

    let om = [(Rc::clone(&key), AccessType::ReadOnly)].into();
    let mom = MeteredOrdMap::from_map(om, &budget)?;
    let mut fp = Footprint(mom);
    assert!(fp
        .enforce_access(&key2, AccessType::ReadOnly, &budget)
        .is_err());
    fp.enforce_access(&key, AccessType::ReadOnly, &budget)?;
    fp.0 =
        fp.0.insert(Rc::clone(&key), AccessType::ReadWrite, &budget)?;
    fp.enforce_access(&key, AccessType::ReadOnly, &budget)?;
    fp.enforce_access(&key, AccessType::ReadWrite, &budget)?;
    Ok(())
}

#[test]
fn footprint_enforce_access_not_exist() -> Result<(), HostError> {
    let budget = Budget::default();
    let mut fp = Footprint::default();
    let key = Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
        contract: ScAddress::Contract(ContractId([0; 32].into())),
        key: ScVal::I32(0),
        durability: ContractDataDurability::Persistent,
    }));
    let res = fp.enforce_access(&key, AccessType::ReadOnly, &budget);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Storage, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

#[test]
fn footprint_attempt_to_write_readonly_entry() -> Result<(), HostError> {
    let budget = Budget::default();
    let key = Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
        contract: ScAddress::Contract(ContractId([0; 32].into())),
        key: ScVal::I32(0),
        durability: ContractDataDurability::Persistent,
    }));
    let om = [(Rc::clone(&key), AccessType::ReadOnly)].into();
    let mom = MeteredOrdMap::from_map(om, &budget)?;
    let mut fp = Footprint(mom);
    let res = fp.enforce_access(&key, AccessType::ReadWrite, &budget);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Storage, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

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
                    test_vec![host, key_1].into(),
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
        test_vec![host, key_1, 1234_u64].into(),
    )
    .unwrap();

    assert_eq!(
        bool::try_from_val(
            host,
            &host
                .call(
                    contract_id,
                    storage_fn_name(host, "has", storage),
                    test_vec![host, key_1].into(),
                )
                .unwrap()
        )
        .unwrap(),
        true
    );

    let max_live_until_ledger: u32 = host.max_live_until_ledger().unwrap();
    let ledger_seq: u32 = host.get_ledger_sequence().unwrap().into();
    let max_extend = max_live_until_ledger - ledger_seq;
    let threshold: u32 = 1;

    // Smoke test extend
    let extend_args = if storage == "instance" {
        test_vec![host, threshold, max_extend]
    } else {
        test_vec![host, key_1, threshold, max_extend]
    };

    host.call(
        contract_id,
        storage_fn_name(host, "extend", storage),
        extend_args.into(),
    )
    .unwrap();

    let extend_args_past_max = if storage == "instance" {
        test_vec![host, threshold, max_extend + 1]
    } else {
        test_vec![host, key_1, threshold, max_extend + 1]
    };

    if storage == "temporary" {
        assert!(host
            .call(
                contract_id,
                storage_fn_name(host, "extend", storage),
                extend_args_past_max.into(),
            )
            .is_err());
    } else {
        host.call(
            contract_id,
            storage_fn_name(host, "extend", storage),
            extend_args_past_max.into(),
        )
        .unwrap();
    }

    // Put another key and verify it's there
    host.call(
        contract_id,
        storage_fn_name(host, "put", storage),
        test_vec![host, key_2, u64::MAX].into(),
    )
    .unwrap();
    assert_eq!(
        bool::try_from_val(
            host,
            &host
                .call(
                    contract_id,
                    storage_fn_name(host, "has", storage),
                    test_vec![
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
                    test_vec![host, key_1].into(),
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
                    test_vec![host, key_2].into(),
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
        test_vec![host, key_2, 4321_u64].into(),
    )
    .unwrap();
    assert_eq!(
        u64::try_from_val(
            host,
            &host
                .call(
                    contract_id,
                    storage_fn_name(host, "get", storage),
                    test_vec![host, key_2].into(),
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
        test_vec![host, key_1].into(),
    )
    .unwrap();
    // Delete again - that's a no-op, but it shouldn't fail either.
    host.call(
        contract_id,
        storage_fn_name(host, "del", storage),
        test_vec![host, key_1].into(),
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
                    test_vec![host, key_1].into(),
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
                    test_vec![host, key_2].into(),
                )
                .unwrap()
        )
        .unwrap(),
        true
    );
}

#[test]
fn test_persistent_storage() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
    test_storage(&host, contract_id, "persistent");
}

#[test]
fn test_temp_storage() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
    test_storage(&host, contract_id, "temporary");
}

#[test]
fn test_instance_storage() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
    test_storage(&host, contract_id, "instance");
}

#[test]
fn test_nested_bump() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let invoke_contract_id = host.register_test_contract_wasm(INVOKE_CONTRACT);
    let storage_contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);

    let contract_id_hash = host.contract_id_from_address(storage_contract_id).unwrap();
    let storage_key: std::rc::Rc<LedgerKey> = host
        .contract_instance_ledger_key(&contract_id_hash)
        .unwrap();
    host.with_mut_storage(|s: &mut Storage| {
        let v = s
            .map
            .get::<Rc<LedgerKey>>(&storage_key, host.as_budget())
            .unwrap()
            .unwrap()
            .clone()
            .unwrap()
            .1
            .unwrap();
        assert_eq!(v, 4095);
        Ok(())
    })
    .unwrap();

    host.call(
        invoke_contract_id,
        Symbol::try_from_val(&*host, &"invoke_storage").unwrap(),
        test_vec![
            &*host,
            storage_contract_id,
            &Symbol::try_from_val(&*host, &"extend_instance").unwrap(),
            5000u32,
            5000u32
        ]
        .into(),
    )
    .unwrap();

    host.with_mut_storage(|s: &mut Storage| {
        let v = s
            .map
            .get::<Rc<LedgerKey>>(&storage_key, host.as_budget())
            .unwrap()
            .unwrap()
            .clone()
            .unwrap()
            .1
            .unwrap();
        // The inner call adds 10 to the extend_to parameter
        assert_eq!(v, 5010);
        Ok(())
    })
    .unwrap()
}

#[test]
fn test_muxed_account_is_not_allowed_as_storage_key() {
    let host = Host::test_host_with_recording_footprint();
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE_WITH_VALS);
    let muxed_address = MuxedScAddress(ScAddress::MuxedAccount(MuxedEd25519Account {
        id: 123,
        ed25519: Uint256([10; 32]),
    }));
    let muxed_address_val = host.add_host_object(muxed_address.clone()).unwrap();

    let run_test = |storage: &str| {
        // Muxed address can't be used as a storage key.
        if storage != "instance" {
            // For instance storage we allow checking for presence of
            // MuxedAddress as ledger key, so there won't be a storage error
            // in a test.
            // We still don't allow storing the MuxedAddresses, so there is no
            // real risk for the users.
            assert!(HostError::result_matches_err(
                host.call(
                    contract_id,
                    storage_fn_name(&host, "get", storage),
                    test_vec![&host, muxed_address_val].into(),
                ),
                (ScErrorType::Storage, ScErrorCode::InvalidInput)
            ));
        }

        assert!(HostError::result_matches_err(
            host.call(
                contract_id,
                storage_fn_name(&host, "put", storage),
                test_vec![&host, muxed_address_val, 1234_u64].into(),
            ),
            (ScErrorType::Storage, ScErrorCode::InvalidInput)
        ));
        // But it can be used as a value
        assert!(host
            .call(
                contract_id,
                storage_fn_name(&host, "put", storage),
                test_vec![&host, 1234_u64, muxed_address_val].into(),
            )
            .is_ok());
        let v = host
            .call(
                contract_id,
                storage_fn_name(&host, "get", storage),
                test_vec![&host, 1234_u64].into(),
            )
            .unwrap();
        let addr_obj = MuxedAddressObject::try_from_val(&host, &v).unwrap();
        let addr = host
            .visit_obj(addr_obj, |addr: &MuxedScAddress| Ok(addr.clone()))
            .unwrap();
        assert_eq!(addr, muxed_address);
    };
    run_test("persistent");
    run_test("temporary");
    run_test("instance");
}

#[test]
fn test_storage_mix() {
    // This makes sure the keyspaces are not mixed between storage types.
    let host = observe_host!(Host::test_host_with_recording_footprint());
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

#[test]
#[should_panic(
    expected = "called `Result::unwrap()` on an `Err` value: Error(Budget, ExceededLimit)"
)]
fn test_large_persistent_value() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
    let key_1 = Symbol::try_from_small_str("key_1").unwrap();

    let bytes = vec![0u8; 100000000];
    let _ = host.try_call(
        contract_id,
        storage_fn_name(&host, "put", "persistent"),
        test_vec![&*host, key_1, bytes].into(),
    );
}

#[test]
#[should_panic(
    expected = "called `Result::unwrap()` on an `Err` value: Error(Budget, ExceededLimit)"
)]
fn test_large_instance_value() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
    let key_1 = Symbol::try_from_small_str("key_1").unwrap();

    let bytes = vec![0u8; 100000000];
    let _ = host.try_call(
        contract_id,
        storage_fn_name(&host, "put", "instance"),
        test_vec![&*host, key_1, bytes].into(),
    );
}

#[test]
#[should_panic(
    expected = "called `Result::unwrap()` on an `Err` value: Error(Budget, ExceededLimit)"
)]
fn test_large_persistent_key() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);

    let key = vec![0u8; 100000000];
    let _ = host.try_call(
        contract_id,
        storage_fn_name(&host, "put", "persistent"),
        test_vec![&*host, key, 1_u64].into(),
    );
}

#[test]
#[should_panic(
    expected = "called `Result::unwrap()` on an `Err` value: Error(Budget, ExceededLimit)"
)]
fn test_large_instance_key() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);

    let key = vec![0u8; 100000000];
    let _ = host.try_call(
        contract_id,
        storage_fn_name(&host, "put", "instance"),
        test_vec![&*host, key, 1_u64].into(),
    );
}

// ============================================================================
// Tests for CAP-0078: extend_contract_data_ttl_v2 and
// extend_contract_instance_and_code_ttl_v2
// ============================================================================

mod ttl_extension_v2_tests {
    use super::*;
    use crate::xdr::{ContractDataDurability, LedgerKeyContractData, ScSymbol, ScVal};
    use soroban_env_common::{ContractTTLExtension, StorageType};
    use soroban_test_wasms::CONTRACT_STORAGE_P26;

    fn setup_host() -> Host {
        let host = Host::test_host_with_recording_footprint();
        host.set_ledger_info(crate::LedgerInfo {
            protocol_version: Host::current_test_protocol(),
            sequence_number: 1000,
            max_entry_ttl: 100000,
            ..Default::default()
        })
        .unwrap();
        host
    }

    fn get_live_until(host: &Host, key: &Rc<LedgerKey>) -> u32 {
        host.with_mut_storage(|s| Ok(s.get_with_live_until_ledger(key, host, None)?.1.unwrap()))
            .unwrap()
    }

    fn make_data_key(host: &Host, contract_id: AddressObject, storage: &str) -> Rc<LedgerKey> {
        let contract_id_hash = host.contract_id_from_address(contract_id).unwrap();
        let durability = match storage {
            "persistent" => ContractDataDurability::Persistent,
            "temporary" => ContractDataDurability::Temporary,
            _ => panic!("unexpected storage type"),
        };
        Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
            contract: ScAddress::Contract(contract_id_hash),
            key: ScVal::Symbol(ScSymbol("key_1".try_into().unwrap())),
            durability,
        }))
    }

    fn extend_v2_fn_name(host: &Host, storage: &str) -> Symbol {
        let name = format!("extend_{}_v2", storage);
        name.as_str().try_into_val(host).unwrap()
    }

    fn test_extend_ttl_v2(host: &Host, contract_id: AddressObject, storage: &str) {
        let key = Symbol::try_from_small_str("key_1").unwrap();

        host.call(
            contract_id,
            storage_fn_name(host, "put", storage),
            test_vec![host, key, 1234_u64].into(),
        )
        .unwrap();

        let storage_key = if storage == "instance" {
            let c_id = host.contract_id_from_address(contract_id).unwrap();
            host.contract_instance_ledger_key(&c_id).unwrap()
        } else {
            make_data_key(host, contract_id, storage)
        };

        let extend = |extend_to: u32, min_ext: u32, max_ext: u32| {
            let curr_live_until = get_live_until(host, &storage_key);
            let args = if storage == "instance" {
                test_vec![host, extend_to, min_ext, max_ext]
            } else {
                test_vec![host, key, extend_to, min_ext, max_ext]
            };
            host.call(contract_id, extend_v2_fn_name(host, storage), args.into())
                .and_then(|_| Ok(get_live_until(host, &storage_key) - curr_live_until))
        };

        // max_extension < min_extension => InvalidInput
        assert!(HostError::result_matches_err(
            extend(5000, 1000, 500),
            (ScErrorType::Storage, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            extend(1, 3, 2),
            (ScErrorType::Storage, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            extend(100, 100, 99),
            (ScErrorType::Storage, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            extend(100, u32::MAX, u32::MAX - 1),
            (ScErrorType::Storage, ScErrorCode::InvalidInput)
        ));

        let ledger_seq: u32 = host.get_ledger_sequence().unwrap().into();
        let curr_ttl = || get_live_until(host, &storage_key) - ledger_seq;

        // regular extension, between min and max
        assert_eq!(extend(curr_ttl() + 500, 100, 1000).unwrap(), 500);
        assert_eq!(extend(curr_ttl() + 1, 0, 1).unwrap(), 1);
        assert_eq!(extend(curr_ttl() + 10, 10, 10).unwrap(), 10);
        assert_eq!(extend(curr_ttl() + 99, 99, 100).unwrap(), 99);
        assert_eq!(extend(curr_ttl() + 90, 40, u32::MAX).unwrap(), 90);

        // no-op when extend_to already satisfied
        assert_eq!(extend(curr_ttl() - 100, 0, 1000).unwrap(), 0);
        assert_eq!(extend(curr_ttl() - 1, 0, 1000).unwrap(), 0);
        assert_eq!(extend(curr_ttl(), 0, 10000).unwrap(), 0);

        // no-op when extension < min_extension
        assert_eq!(extend(curr_ttl() + 50, 100, 200).unwrap(), 0);
        assert_eq!(extend(curr_ttl(), 1, 10000).unwrap(), 0); // extension = 0
        assert_eq!(extend(curr_ttl() + 99, 100, 10000).unwrap(), 0); // extension = min - 1

        // extension capped by max_extension
        assert_eq!(extend(curr_ttl() + 4999, 4999, 4999).unwrap(), 4999);
        assert_eq!(extend(curr_ttl() + 3000, 199, 200).unwrap(), 200);
        assert_eq!(extend(curr_ttl() + 7000, 0, 1).unwrap(), 1);

        // let curr_live_until = get_live_until(host, &storage_key);
        // let max_live_until = host.max_live_until_ledger().unwrap();

        let max_ttl = host.max_live_until_ledger().unwrap() - ledger_seq;
        // Extend exactly to max_live_until
        let max_ext = max_ttl - curr_ttl();
        assert_eq!(extend(max_ttl, 0, 100000).unwrap(), max_ext);

        // For temporary storage, test extensions that should error due to
        // exceeding the maximum TTL allowed by config.
        if storage == "temporary" {
            for _ in 0..=1 {
                // 1 ledger past max extension
                assert!(HostError::result_matches_err(
                    extend(max_ttl + 1, 0, max_ttl + 100),
                    (ScErrorType::Storage, ScErrorCode::InvalidAction)
                ));
                // TTL is past max, but below min extension, should still error.
                assert!(HostError::result_matches_err(
                    extend(max_ttl + 100, max_ttl, max_ttl + 10000),
                    (ScErrorType::Storage, ScErrorCode::InvalidAction)
                ));
                // Extension is past max, but is below the max_extension argument,
                // should still error.
                assert!(HostError::result_matches_err(
                    extend(max_ttl + 100, 0, 1),
                    (ScErrorType::Storage, ScErrorCode::InvalidAction)
                ));
                // Bump ledger sequence such that there is an extension
                // opportunity and ensure the errors persist.
                host.with_mut_ledger_info(|li| {
                    li.sequence_number += 1000;
                })
                .unwrap();
            }
        } else {
            // For persistent/instance storage, extensions are clamped to not
            // exceed max TTL allowed.

            // Try extending past max, these should be no-ops.
            assert_eq!(extend(max_ttl + 1, 0, max_ttl + 100).unwrap(), 0);
            assert_eq!(extend(max_ttl + 1000, 0, u32::MAX).unwrap(), 0);
            // No-op due to too high min extension in addition to being extended
            // to max.
            assert_eq!(
                extend(max_ttl + 1000, max_ttl + 10000, u32::MAX).unwrap(),
                0
            );

            // Bump ledger sequence to get some actual extensions.
            host.with_mut_ledger_info(|li| {
                li.sequence_number += 1000;
            })
            .unwrap();
            // Bump by just 1 ledger (guarded with max_extension).
            assert_eq!(extend(max_ttl + 1, 0, 1).unwrap(), 1);
            // No bump due to min extension too high.
            assert_eq!(extend(max_ttl + 100, max_ttl, max_ttl + 1000).unwrap(), 0);
            // Bump without min/max restrictions to reach max TTL.
            assert_eq!(extend(max_ttl + 1000, 0, max_ttl + 100).unwrap(), 999);
        }
    }

    #[test]
    fn test_extend_ttl_v2_persistent() {
        let host = setup_host();
        let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE_P26);
        test_extend_ttl_v2(&host, contract_id, "persistent");
    }

    #[test]
    fn test_extend_ttl_v2_instance() {
        let host = setup_host();
        let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE_P26);
        test_extend_ttl_v2(&host, contract_id, "instance");
    }

    #[test]
    fn test_extend_ttl_v2_temporary() {
        let host = setup_host();
        let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE_P26);
        test_extend_ttl_v2(&host, contract_id, "temporary");
    }

    #[test]
    fn test_extend_data_ttl_v2_rejects_instance_storage() {
        let host = setup_host();
        let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE_P26);
        let key = Symbol::try_from_small_str("key_1").unwrap();
        host.call(
            contract_id,
            storage_fn_name(&host, "put", "instance"),
            test_vec![&host, key, 1234_u64].into(),
        )
        .unwrap();

        let key_val = host
            .to_host_val(&ScVal::Symbol(ScSymbol("key_1".try_into().unwrap())))
            .unwrap();
        assert!(HostError::result_matches_err(
            host.extend_contract_data_ttl_v2(
                key_val,
                StorageType::Instance,
                5000.into(),
                1.into(),
                500.into()
            ),
            (ScErrorType::Storage, ScErrorCode::InvalidAction)
        ));
    }

    #[test]
    fn test_extend_instance_and_code_ttl_v2_extension_scope() {
        let host = setup_host();
        let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE_P26);
        let contract_id_hash = host.contract_id_from_address(contract_id).unwrap();
        let instance_key = host
            .contract_instance_ledger_key(&contract_id_hash)
            .unwrap();

        let code_hash = match &host
            .retrieve_contract_instance_from_storage(&instance_key)
            .unwrap()
            .executable
        {
            crate::xdr::ContractExecutable::Wasm(hash) => hash.clone(),
            _ => panic!("Expected Wasm executable"),
        };
        let code_key = host.contract_code_ledger_key(&code_hash).unwrap();

        let get_ttls = || {
            (
                get_live_until(&host, &instance_key),
                get_live_until(&host, &code_key),
            )
        };

        let extend = |scope: ContractTTLExtension, extend_to: u32| {
            let (inst_before, code_before) = get_ttls();
            host.extend_contract_instance_and_code_ttl_v2(
                contract_id,
                scope,
                extend_to.into(),
                1.into(),
                500.into(),
            )
            .unwrap();
            let (inst_after, code_after) = get_ttls();
            (inst_after - inst_before, code_after - code_before)
        };

        assert_eq!(
            extend(ContractTTLExtension::InstanceAndCode, 8000),
            (500, 500)
        );
        assert_eq!(extend(ContractTTLExtension::Instance, 9000), (500, 0));
        assert_eq!(extend(ContractTTLExtension::Code, 10000), (0, 500));
    }
}
