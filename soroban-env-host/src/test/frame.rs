use crate::{host::HostError, ContractFunctionSet, Host};
use soroban_env_common::{
    xdr::{Hash, ScAddress},
    Symbol, Val,
};
use std::rc::Rc;

#[test]
fn has_frame() -> Result<(), HostError> {
    struct NoopContractFunctionSet;
    impl ContractFunctionSet for NoopContractFunctionSet {
        fn call(&self, _func: &Symbol, _host: &Host, _args: &[Val]) -> Option<Val> {
            None
        }
    }

    let host = observe_host!(Host::test_host_with_recording_footprint());

    // Host has no frame outside of executing a contract.
    assert!(!host.has_frame()?);

    // Host has a frame when executing a contract.
    let id = [0u8; 32];
    let address = host.add_host_object(ScAddress::Contract(Hash(id)))?;
    host.register_test_contract(address, Rc::new(NoopContractFunctionSet))?;
    let func = Symbol::try_from_small_str("")?;
    host.with_test_contract_frame(Hash(id), func, || {
        assert!(host.has_frame()?);
        Ok(().into())
    })?;

    // Host has no frame outside of executing a contract.
    assert!(!host.has_frame()?);

    Ok(())
}
