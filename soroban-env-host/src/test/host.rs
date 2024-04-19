use soroban_env_common::xdr::{ScBytes, ScErrorCode, ScErrorType};
use soroban_env_common::Val;

use crate::{
    budget::Budget,
    storage::{Footprint, Storage, StorageMap},
    Env, Host, HostError,
};

#[test]
fn invalid_object_handles() -> Result<(), HostError> {
    let create_host = || -> Result<Host, HostError> {
        let budget = Budget::default();
        let storage =
            Storage::with_enforcing_footprint_and_map(Footprint::default(), StorageMap::new());

        let host = Host::with_storage_and_budget(storage, budget);
        host.set_test_ledger_info_with_current_test_protocol();

        Ok(host)
    };

    let host1 = create_host()?;
    let host2 = create_host()?;

    // Create the object in host1, and then try to fetch from host2
    let obj = host2.get_ledger_network_id()?;
    let err = host1
        .visit_obj(obj, |np: &ScBytes| Ok(np.to_vec()))
        .err()
        .unwrap();

    assert!(err.error.is_type(ScErrorType::Value));
    assert!(err.error.is_code(ScErrorCode::InvalidInput));

    Ok(())
}

#[test]
fn invalid_val() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let v = Val::from_payload(u64::MAX);
    let err = host.from_host_val(v).err().unwrap();

    assert!(err.error.is_type(ScErrorType::Value));
    assert!(err.error.is_code(ScErrorCode::UnexpectedType));

    Ok(())
}
