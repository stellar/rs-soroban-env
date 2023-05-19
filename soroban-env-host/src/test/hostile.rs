use soroban_env_common::xdr::{ScErrorCode, ScErrorType};
use soroban_test_wasms::HOSTILE;

use crate::{
    xdr::{Hash, ScVec},
    Host, HostError, Vm,
};

#[test]
fn hostile_iloop_traps() -> Result<(), HostError> {
    let host = Host::default();
    let id: Hash = [0; 32].into();
    let vm = Vm::new(&host, id, HOSTILE)?;
    let args: ScVec = host.test_scvec::<i32>(&[])?;

    let res = vm.invoke_function(&host, "iloop", &args);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

#[test]
fn hostile_badack_traps() -> Result<(), HostError> {
    let host = Host::default();
    let id: Hash = [0; 32].into();
    let vm = Vm::new(&host, id, HOSTILE)?;
    let args: ScVec = host.test_scvec::<i32>(&[])?;

    let res = vm.invoke_function(&host, "badack", &args);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

#[test]
fn hostile_ssmash_traps() -> Result<(), HostError> {
    let host = Host::default();
    let id: Hash = [0; 32].into();
    let vm = Vm::new(&host, id, HOSTILE)?;
    let args: ScVec = host.test_scvec::<i32>(&[])?;

    let res = vm.invoke_function(&host, "ssmash", &args);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::InternalError)
    ));
    Ok(())
}

#[test]
fn hostile_oob1_traps() -> Result<(), HostError> {
    let host = Host::default();
    let id: Hash = [0; 32].into();
    let vm = Vm::new(&host, id, HOSTILE)?;
    let args: ScVec = host.test_scvec::<i32>(&[])?;

    let res = vm.invoke_function(&host, "oob1", &args);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::InternalError)
    ));
    Ok(())
}

#[test]
fn hostile_oob2_traps() -> Result<(), HostError> {
    let host = Host::default();
    let id: Hash = [0; 32].into();
    let vm = Vm::new(&host, id, HOSTILE)?;
    let args: ScVec = host.test_scvec::<i32>(&[])?;

    let res = vm.invoke_function(&host, "oob2", &args);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::InternalError)
    ));
    Ok(())
}

#[test]
fn hostile_objs_traps() -> Result<(), HostError> {
    let host = Host::default();
    let id: Hash = [0; 32].into();
    let vm = Vm::new(&host, id, HOSTILE)?;
    let args: ScVec = host.test_scvec::<i32>(&[])?;

    host.set_diagnostic_level(crate::DiagnosticLevel::Debug);
    host.with_budget(|b| Ok(b.reset_default()))?;
    host.with_budget(|b| Ok(b.reset_unlimited_cpu()))?;

    // This one should just run out of memory
    let res = vm.invoke_function(&host, "objs", &args);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}
