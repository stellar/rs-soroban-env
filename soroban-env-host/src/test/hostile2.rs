use soroban_env_common::xdr::{ScErrorCode, ScErrorType};
use soroban_test_wasms::HOSTILE2;

use crate::{Host, Symbol, HostError};
use crate::native_contract::base_types::Vec as HostVec;
use soroban_env_common::Env;

// Regression test for debug-level logging outside of memory range.
#[test]
fn hostile_large_log_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    host.enable_debug();
    let contract_id = host.register_test_contract_wasm(HOSTILE2);

    let res = host.call(
        contract_id,
        Symbol::try_from_small_str("largelog").unwrap(),
        HostVec::new(&host).unwrap().into(),
    );

    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::IndexBounds)
    ));

    Ok(())
}
