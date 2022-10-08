use soroban_test_wasms::HOSTILE;

use crate::{
    xdr::{Hash, ScVec},
    Host, HostError, Vm,
};

#[test]
fn hostile_functions_all_trap() -> Result<(), HostError> {
    use soroban_env_common::xdr::ScVmErrorCode;

    let host = Host::default();
    let id: Hash = [0; 32].into();
    let vm = Vm::new(&host, id, HOSTILE)?;
    let args: ScVec = host.test_scvec::<i32>(&[])?;

    let res = vm.invoke_function(&host, "iloop", &args);
    assert!(HostError::result_matches_err_status(
        res,
        ScVmErrorCode::TrapCpuLimitExceeded
    ));

    host.with_budget(|b| b.reset_default());
    let res = vm.invoke_function(&host, "badack", &args);
    assert!(HostError::result_matches_err_status(
        res,
        ScVmErrorCode::TrapCpuLimitExceeded
    ));

    host.with_budget(|b| b.reset_default());
    let res = vm.invoke_function(&host, "ssmash", &args);
    assert!(HostError::result_matches_err_status(
        res,
        ScVmErrorCode::TrapUnreachable
    ));

    host.with_budget(|b| b.reset_default());
    let res = vm.invoke_function(&host, "oob1", &args);
    assert!(HostError::result_matches_err_status(
        res,
        ScVmErrorCode::TrapUnreachable
    ));

    host.with_budget(|b| b.reset_default());
    let res = vm.invoke_function(&host, "oob2", &args);
    assert!(HostError::result_matches_err_status(
        res,
        ScVmErrorCode::TrapUnreachable
    ));

    host.with_budget(|b| b.reset_default());
    host.with_budget(|b| b.reset_unlimited_cpu());
    let res = vm.invoke_function(&host, "objs", &args);
    assert!(HostError::result_matches_err_status(
        res,
        ScVmErrorCode::TrapMemLimitExceeded
    ));

    Ok(())
}
