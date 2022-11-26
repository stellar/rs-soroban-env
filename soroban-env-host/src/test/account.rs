use std::rc::Rc;

use soroban_env_common::CheckedEnv;

use crate::{
    budget::Budget,
    host::metered_map::MeteredOrdMap,
    storage::{AccessType, Footprint, Storage},
    xdr, Host, HostError, RawVal,
};

#[test]
fn check_account_exists() -> Result<(), HostError> {
    let acc_id0 = xdr::AccountId(xdr::PublicKey::PublicKeyTypeEd25519(xdr::Uint256([0; 32])));
    let acc_id1 = xdr::AccountId(xdr::PublicKey::PublicKeyTypeEd25519(xdr::Uint256([1; 32]))); // declared, but not in storage
    let acc_id2 = xdr::AccountId(xdr::PublicKey::PublicKeyTypeEd25519(xdr::Uint256([2; 32]))); // not declared
    let (lk0, le0) = Host::test_account_ledger_key_entry_pair(acc_id0.clone());
    let (lk1, _le1) = Host::test_account_ledger_key_entry_pair(acc_id1.clone());
    let (_lk2, _le2) = Host::test_account_ledger_key_entry_pair(acc_id2.clone());

    let budget = Budget::default();
    let mut footprint = Footprint::default();
    footprint.record_access(&lk0, AccessType::ReadOnly, &budget)?;
    footprint.record_access(&lk1, AccessType::ReadOnly, &budget)?;

    let mut map = Vec::new();
    map.push((Rc::new(lk0), Some(Rc::new(le0))));
    let storage = Storage::with_enforcing_footprint_and_map(
        footprint,
        MeteredOrdMap::from_map(map, &budget)?,
    );

    let host = Host::with_storage_and_budget(storage, budget.clone());
    let obj0 = host.add_host_object(acc_id0)?;
    let obj1 = host.add_host_object(acc_id1)?;
    let obj2 = host.add_host_object(acc_id2)?;
    // declared and exists
    assert_eq!(
        host.account_exists(obj0)?.get_payload(),
        RawVal::from_bool(true).get_payload()
    );
    // declared but does not exist
    assert_eq!(
        host.account_exists(obj1)?.get_payload(),
        RawVal::from_bool(false).get_payload()
    );
    // not declared
    assert!(HostError::result_matches_err_status(
        host.account_exists(obj2),
        xdr::ScHostStorageErrorCode::AccessToUnknownEntry
    ));
    Ok(())
}
