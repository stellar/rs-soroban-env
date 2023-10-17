use std::rc::Rc;

use crate::budget::Budget;
use crate::native_contract::testutils::HostVec;
use crate::storage::{AccessType, Footprint};
use crate::xdr::{
    ContractDataDurability, LedgerKey, LedgerKeyContractData, ScAddress, ScErrorCode, ScErrorType,
    ScVal,
};
use crate::{host_vec, Host, HostError, MeteredOrdMap};
use soroban_env_common::{AddressObject, Env, Symbol, TryFromVal, TryIntoVal};
use soroban_test_wasms::CONTRACT_STORAGE;

#[test]
fn footprint_record_access() -> Result<(), HostError> {
    let budget = Budget::default();
    budget.reset_unlimited()?;
    let mut fp = Footprint::default();
    // record when key not exist
    let key = Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
        contract: ScAddress::Contract([0; 32].into()),
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
        contract: ScAddress::Contract([0; 32].into()),
        key: ScVal::I32(0),
        durability: ContractDataDurability::Persistent,
    }));

    // Key not in footprint. Only difference is type_
    let key2 = Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
        contract: ScAddress::Contract([0; 32].into()),
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
        contract: ScAddress::Contract([0; 32].into()),
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
        contract: ScAddress::Contract([0; 32].into()),
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

    let max_live_until_ledger: u32 = host.max_live_until_ledger().unwrap().into();
    let ledger_seq: u32 = host.get_ledger_sequence().unwrap().into();
    let max_extend = max_live_until_ledger - ledger_seq;
    let threshold: u32 = 1;

    // Smoke test extend
    let extend_args = if storage == "instance" {
        host_vec![host, threshold, max_extend]
    } else {
        host_vec![host, key_1, threshold, max_extend]
    };

    host.call(
        contract_id,
        storage_fn_name(host, "extend", storage),
        extend_args.into(),
    )
    .unwrap();

    let extend_args_past_max = if storage == "instance" {
        host_vec![host, threshold, max_extend + 1]
    } else {
        host_vec![host, key_1, threshold, max_extend + 1]
    };

    assert!(host
        .call(
            contract_id,
            storage_fn_name(host, "extend", storage),
            extend_args_past_max.into(),
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
