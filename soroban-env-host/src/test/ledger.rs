use soroban_env_common::{xdr::ScBytes, Env};

use crate::{
    budget::Budget,
    storage::{Footprint, Storage, StorageMap},
    Host, HostError,
};

#[test]
fn ledger_network_id() -> Result<(), HostError> {
    let budget = Budget::default();
    let storage =
        Storage::with_enforcing_footprint_and_map(Footprint::default(), StorageMap::new());

    let host = Host::with_storage_and_budget(storage, budget);
    host.set_test_ledger_info_with_current_test_protocol();
    host.with_mut_ledger_info(|li| {
        li.network_id = [7; 32];
    })?;
    let obj = host.get_ledger_network_id()?;
    let np = host.visit_obj(obj, |np: &ScBytes| Ok(np.to_vec()))?;
    assert_eq!(np, vec![7; 32],);
    Ok(())
}
