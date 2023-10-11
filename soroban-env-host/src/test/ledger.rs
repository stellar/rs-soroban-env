use soroban_env_common::{xdr::ScBytes, Env};

use crate::{
    budget::Budget,
    storage::{Footprint, Storage, StorageMap},
    Host, HostError, LedgerInfo,
};

#[test]
fn ledger_network_id() -> Result<(), HostError> {
    let budget = Budget::default();
    let storage =
        Storage::with_enforcing_footprint_and_map(Footprint::default(), StorageMap::new());

    let host = Host::with_storage_and_budget(storage, budget);
    host.set_ledger_info(LedgerInfo {
        protocol_version: crate::meta::get_ledger_protocol_version(crate::meta::INTERFACE_VERSION),
        sequence_number: 0,
        timestamp: 0,
        network_id: [7; 32],
        base_reserve: 0,
        min_persistent_entry_ttl: 4096,
        min_temp_entry_ttl: 16,
        max_entry_ttl: 6312000,
    })?;
    let obj = host.get_ledger_network_id()?;
    let np = host.visit_obj(obj, |np: &ScBytes| Ok(np.to_vec()))?;
    assert_eq!(np, vec![7; 32],);
    Ok(())
}
