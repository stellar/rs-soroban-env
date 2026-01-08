use soroban_env_common::{Env, StorageType};

use crate::{
    host::HostError,
    xdr::{ContractId, Hash, ScAddress, ScErrorCode, ScErrorType},
    Compare, ContractFunctionSet, EnvBase, Error, Host, Symbol, Val,
};

use std::rc::Rc;

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

#[test]
fn has_frame() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());

    // Host has no frame outside of executing a contract.
    assert!(!host.has_frame()?);

    // Host has a frame when executing a contract.
    let id = [0u8; 32];
    let address = host.add_host_object(ScAddress::Contract(ContractId(Hash(id))))?;
    host.register_test_contract(address, Rc::new(NoopContractFunctionSet))?;
    host.with_test_contract_frame(
        ContractId(Hash(id)),
        Symbol::try_from_small_str("").unwrap(),
        || {
            assert!(host.has_frame()?);
            Ok(().into())
        },
    )?;
    // Host has no frame outside of executing a contract.
    assert!(!host.has_frame()?);

    Ok(())
}

#[test]
fn try_with_test_contract_frame_has_frame() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());

    // Host has no frame outside of executing a contract.
    assert!(!host.has_frame()?);

    // Host has a frame when executing a contract.
    let id = [0u8; 32];
    let address = host.add_host_object(ScAddress::Contract(ContractId(Hash(id))))?;
    host.register_test_contract(address, Rc::new(NoopContractFunctionSet))?;
    host.try_with_test_contract_frame(
        ContractId(Hash(id)),
        Symbol::try_from_small_str("test").unwrap(),
        || {
            assert!(host.has_frame()?);
            Ok(().into())
        },
    )?;

    // Host has no frame outside of executing a contract.
    assert!(!host.has_frame()?);

    Ok(())
}

#[test]
fn try_with_test_contract_frame_catches_host_error() -> Result<(), HostError> {
    // Setup host and contract
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let id = [0u8; 32];
    let address = host.add_host_object(ScAddress::Contract(ContractId(Hash(id))))?;
    host.register_test_contract(address, Rc::new(NoopContractFunctionSet))?;

    // Cause a HostError inside the contract frame
    let result = host.try_with_test_contract_frame(
        ContractId(Hash(id)),
        Symbol::try_from_small_str("test").unwrap(),
        || {
            let key = host.symbol_new_from_slice(b"nonexistent")?;
            host.get_contract_data(key.as_val().clone(), StorageType::Persistent)?;
            Ok(().into())
        },
    );
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err(),
        HostError::from(Error::from_type_and_code(
            ScErrorType::Storage,
            ScErrorCode::MissingValue
        ))
    );

    Ok(())
}

#[test]
fn try_with_test_contract_frame_catches_panic() -> Result<(), HostError> {
    // Setup host and contract
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.set_diagnostic_level(crate::DiagnosticLevel::Debug)?;
    let id = [0u8; 32];
    let address = host.add_host_object(ScAddress::Contract(ContractId(Hash(id))))?;
    host.register_test_contract(address, Rc::new(NoopContractFunctionSet))?;

    // Cause a panic inside the contract frame
    let panic_str = "intentional panic for testing";
    let result = host.try_with_test_contract_frame(
        ContractId(Hash(id)),
        Symbol::try_from_small_str("test").unwrap(),
        || {
            panic!("{panic_str}");
        },
    );
    assert!(result.is_err());

    // Validate that a diagnostic event was logged with the contents of the panic
    let events = host.get_events()?.0;
    let mut has_diagnostic_event = false;
    for (_, event) in events.iter().enumerate() {
        let as_str = format!("{:?}", event);
        if as_str.contains(panic_str) {
            has_diagnostic_event = true;
            break;
        }
    }
    assert!(has_diagnostic_event);

    assert_eq!(
        result.unwrap_err(),
        HostError::from(Error::from_type_and_code(
            ScErrorType::WasmVm,
            ScErrorCode::InvalidAction
        ))
    );

    Ok(())
}
