use std::rc::Rc;

use crate::{
    budget::Budget,
    storage::{SnapshotSource, Storage},
    xdr::Hash,
    Host, HostError, MeteredOrdMap, Vm,
};
use soroban_env_common::xdr::{LedgerEntry, LedgerKey, ScErrorCode, ScErrorType, ScVec};
use soroban_test_wasms::COMPLEX;

struct EmptySnap;
impl SnapshotSource for EmptySnap {
    fn get(&self, _key: &Rc<LedgerKey>) -> Result<Rc<LedgerEntry>, HostError> {
        Err((ScErrorType::Storage, ScErrorCode::MissingValue).into())
    }

    fn has(&self, _key: &Rc<LedgerKey>) -> Result<bool, HostError> {
        Ok(false)
    }
}

#[test]
fn run_complex() -> Result<(), HostError> {
    let info = crate::LedgerInfo {
        protocol_version: 21,
        sequence_number: 1234,
        timestamp: 1234,
        network_id: [7; 32],
        base_reserve: 1,
        min_persistent_entry_expiration: 4096,
        min_temp_entry_expiration: 16,
        max_entry_expiration: 6312000,
    };
    let id: Hash = [0; 32].into();

    // Run 1: record footprint, emulating "preflight".
    let foot = {
        let store = Storage::with_recording_footprint(Rc::new(EmptySnap));
        let host = Host::with_storage_and_budget(store, Budget::default());
        host.set_ledger_info(info.clone());
        {
            let vm = Vm::new(&host, id.clone(), COMPLEX)?;
            let args: ScVec = host.test_scvec::<i32>(&[])?;
            vm.invoke_function(&host, "go", &args)?;
        }
        let (store, _, _, _) = host.try_finish().unwrap();
        store.footprint
    };

    // Run 2: enforce preflight footprint, with empty map -- contract should only write.
    {
        let store = Storage::with_enforcing_footprint_and_map(foot, MeteredOrdMap::default());
        let host = Host::with_storage_and_budget(store, Budget::default());
        host.set_ledger_info(info);
        let vm = Vm::new(&host, id, COMPLEX)?;
        let args: ScVec = host.test_scvec::<i32>(&[])?;
        vm.invoke_function(&host, "go", &args)?;
    }
    Ok(())
}
