use crate::{
    host::HostError,
    xdr::{Hash, ScAddress},
    Compare, ContractFunctionSet, EnvBase, Host, Symbol, Val,
};

use std::rc::Rc;

#[test]
fn has_frame() -> Result<(), HostError> {
    struct NoopContractFunctionSet;
    impl ContractFunctionSet for NoopContractFunctionSet {
        fn call(&self, func: &Symbol, host: &Host, _args: &[Val]) -> Option<Val> {
            if host
                .compare(
                    &host.symbol_new_from_slice(b"__constructor").unwrap().into(),
                    func,
                )
                .unwrap()
                .is_ne()
            {
                None
            } else {
                Some(().into())
            }
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
