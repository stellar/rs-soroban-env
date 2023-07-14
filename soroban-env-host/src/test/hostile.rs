use soroban_env_common::{
    xdr::{ScErrorCode, ScErrorType},
    Env, Symbol,
};
use soroban_test_wasms::HOSTILE;

use crate::{host_object::HostVec, Host, HostError};

#[test]
fn hostile_iloop_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("iloop")?,
        host.add_host_object(HostVec::new())?,
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

#[test]
fn hostile_badack_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("badack")?,
        host.add_host_object(HostVec::new())?,
    );

    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

#[test]
fn hostile_ssmash_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("ssmash")?,
        host.add_host_object(HostVec::new())?,
    );

    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));
    Ok(())
}

#[test]
fn hostile_oob1_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("oob1")?,
        host.add_host_object(HostVec::new())?,
    );

    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));
    Ok(())
}

#[test]
fn hostile_oob2_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("oob2")?,
        host.add_host_object(HostVec::new())?,
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));
    Ok(())
}

#[test]
fn hostile_objs_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    host.set_diagnostic_level(crate::DiagnosticLevel::Debug)?;
    host.with_budget(|b| b.reset_default())?;
    host.with_budget(|b| b.reset_unlimited_cpu())?;

    // This one should just run out of memory
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("objs")?,
        host.add_host_object(HostVec::new())?,
    );

    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}
