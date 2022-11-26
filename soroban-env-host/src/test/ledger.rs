use soroban_env_common::CheckedEnv;

use crate::{
    budget::Budget,
    host::metered_map::MeteredOrdMap,
    storage::{Footprint, Storage},
    Host, HostError, LedgerInfo,
};

#[test]
fn ledger_network_passphrase() -> Result<(), HostError> {
    let budget = Budget::default();
    let storage = Storage::with_enforcing_footprint_and_map(
        Footprint::default(),
        MeteredOrdMap::new(&budget)?,
    );

    let host = Host::with_storage_and_budget(storage, budget);
    host.set_ledger_info(LedgerInfo {
        protocol_version: 0,
        sequence_number: 0,
        timestamp: 0,
        network_passphrase: "Public Global Stellar Network ; September 2015"
            .as_bytes()
            .to_vec(),
        base_reserve: 0,
    });
    let obj = host.get_ledger_network_passphrase()?;
    let np = host.visit_obj(obj, |np: &Vec<u8>| Ok(np.clone()))?;
    assert_eq!(
        np,
        "Public Global Stellar Network ; September 2015"
            .as_bytes()
            .to_vec(),
    );
    Ok(())
}

#[test]
fn ledger_network_id() -> Result<(), HostError> {
    let budget = Budget::default();
    let storage = Storage::with_enforcing_footprint_and_map(
        Footprint::default(),
        MeteredOrdMap::new(&budget)?,
    );

    let host = Host::with_storage_and_budget(storage, budget);
    host.set_ledger_info(LedgerInfo {
        protocol_version: 0,
        sequence_number: 0,
        timestamp: 0,
        network_passphrase: "Public Global Stellar Network ; September 2015"
            .as_bytes()
            .to_vec(),
        base_reserve: 0,
    });
    let obj = host.get_ledger_network_id()?;
    let np = host.visit_obj(obj, |np: &Vec<u8>| Ok(np.clone()))?;
    assert_eq!(
        np,
        bytes_lit::bytes!(0x7ac33997544e3175d266bd022439b22cdb16508c01163f26e5cb2a3e1045a979)
            .to_vec(),
    );
    Ok(())
}
