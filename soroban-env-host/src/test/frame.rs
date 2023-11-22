use soroban_env_common::Symbol;

use crate::{host::HostError, Host};

#[test]
fn in_frame() -> Result<(), HostError> {
    let host = observe_host!(Host::default());

    // Host has no frame outside of executing a contract.
    assert!(!host.has_frame());

    // Host has a frame when executing a contract.
    let id = [0u8; 32];
    let func = Symbol::try_from_small_str("")?;
    host.with_test_contract_frame(id.into(), func, || {
        assert!(host.has_frame());
        Ok(().into())
    })?;

    // Host has no frame outside of executing a contract.
    assert!(!host.has_frame());

    Ok(())
}
