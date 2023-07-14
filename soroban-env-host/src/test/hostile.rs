use soroban_env_common::{
    xdr::{ScErrorCode, ScErrorType},
    Env, EnvBase, Symbol, Val, VecObject,
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
        (ScErrorType::WasmVm, ScErrorCode::InternalError)
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
        (ScErrorType::WasmVm, ScErrorCode::InternalError)
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
        (ScErrorType::WasmVm, ScErrorCode::InternalError)
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

#[test]
fn hostile_forged_objects_trap() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    host.set_diagnostic_level(crate::DiagnosticLevel::Debug)?;
    host.with_budget(|b| b.reset_default())?;
    host.with_budget(|b| b.reset_unlimited_cpu())?;

    fn forged_val_to_forge_call_args(host: &Host, val: Val) -> Result<VecObject, HostError> {
        let payload = val.get_payload();
        let lo = payload as u32;
        let hi = (payload >> 32) as u32;
        host.vec_new_from_slice(&[lo.into(), hi.into()])
    }

    // Here we're passing a vector of two numbers that, when reassembled into a
    // payload and cast to an object, denote an absolute object reference. These
    // should fail because relative-to-absolute conversion will reject it.
    let absolute_vec = host.vec_new_from_slice(&[1u32.into(), 2u32.into()])?;
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("forgeref")?,
        forged_val_to_forge_call_args(&host, absolute_vec.to_val())?,
    );
    assert!(HostError::result_matches_err(
        res.clone(),
        (ScErrorType::Context, ScErrorCode::InvalidInput)
    ));

    // Here we just pick a big handle number -- but with a zero bit set, so it's
    // a relative handle -- to poke around "random object space" to see if we
    // can get an object. This will fail because it doesn't denote anything in
    // the relative object table (it's past the end).
    let big_vec_ref = unsafe { VecObject::from_handle(0xffff0) };
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("forgeref")?,
        forged_val_to_forge_call_args(&host, big_vec_ref.to_val())?,
    );
    assert!(HostError::result_matches_err(
        res.clone(),
        (ScErrorType::Context, ScErrorCode::InvalidInput)
    ));

    // Here we call a function that tries to forge the type of an object
    // reference and call a method on it. This fails in the relative-to-absolute
    // conversion path, where we check identity of types.
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("forgety1")?,
        host.vec_new_from_slice(&[absolute_vec.to_val()])?,
    );
    assert!(HostError::result_matches_err(
        res.clone(),
        (ScErrorType::Object, ScErrorCode::UnexpectedType)
    ));

    // Here we call a function that tries to forge the type of an object
    // reference and just pass it as an _argument_ to another function. This
    // fails in the same place as the previous test.
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("forgety2")?,
        host.vec_new_from_slice(&[absolute_vec.to_val()])?,
    );
    assert!(HostError::result_matches_err(
        res.clone(),
        (ScErrorType::Object, ScErrorCode::UnexpectedType)
    ));

    Ok(())
}
