use soroban_env_common::xdr::{ReadXdr, WriteXdr};

use crate::{
    budget::AsBudget,
    host::metered_clone::MeteredClone,
    xdr::{ScErrorCode, ScErrorType, ScVal, ScVec},
    Env, Host, HostError,
};

#[test]
fn deep_scval_to_host_val() -> Result<(), HostError> {
    let host = Host::default();
    host.as_budget().reset_unlimited()?;

    let mut v = ScVec::default();
    // Having a large value here, e.g. 1000, will still cause a stack overflow,
    // but this should be fine as long as this creation path is not in the host.
    // I.e. the guest can create whatever structure they like, as long as the host
    // limits catch it.
    for _ in 0..300 {
        let vv = ScVec::try_from(vec![ScVal::from(v)])?;
        v = vv;
    }

    let res = host.to_host_val(&ScVal::from(v));
    // NB: this error code is not great, it's a consequence of
    // bug https://github.com/stellar/rs-soroban-env/issues/1046
    // where the TryIntoVal impl in common eats HostErrors
    let code = (ScErrorType::Value, ScErrorCode::UnexpectedType);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn deep_host_val_to_scval() -> Result<(), HostError> {
    let host = Host::default();
    host.as_budget().reset_unlimited()?;

    let mut hv = host.test_vec_obj::<u32>(&[])?;
    for _ in 0..1000 {
        let vv = host.test_vec_obj::<u32>(&[])?;
        hv = host.vec_push_back(vv, hv.to_val())?;
    }
    let res = host.from_host_obj(hv);
    // NB: this error code is not great, it's a consequence of
    // bug https://github.com/stellar/rs-soroban-env/issues/1046
    // where the TryIntoVal impl in common eats HostErrors
    let code = (ScErrorType::Value, ScErrorCode::UnexpectedType);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn deep_host_obj_clone() -> Result<(), HostError> {
    let host = Host::default();
    host.as_budget().reset_unlimited()?;

    let mut v = ScVec::default();
    for _ in 0..1000 {
        let vv = ScVec::try_from(vec![ScVal::from(v)])?;
        v = vv;
    }

    let res = v.metered_clone(host.as_budget());
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn deep_host_obj_cmp() -> Result<(), HostError> {
    let host = Host::default();
    host.as_budget().reset_unlimited()?;

    let mut hv = host.test_vec_obj::<u32>(&[])?;
    for _ in 0..1000 {
        let vv = host.test_vec_obj::<u32>(&[])?;
        hv = host.vec_push_back(vv, hv.to_val())?;
    }

    let mut hv2 = host.test_vec_obj::<u32>(&[])?;
    for _ in 0..1000 {
        let vv = host.test_vec_obj::<u32>(&[])?;
        hv2 = host.vec_push_back(vv, hv2.to_val())?;
    }

    let res = host.obj_cmp(hv.to_val(), hv2.to_val());
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn deep_scval_xdr_serialization() -> Result<(), HostError> {
    let mut v = ScVal::from(ScVec::default());
    for _ in 0..200 {
        let vv = ScVec::try_from(vec![v])?;
        v = ScVal::from(vv);
    }
    let res = v
        .to_xdr_with_depth_limit(500)
        .map_err(|e| HostError::from(e));
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn deep_scval_xdr_deserialization() -> Result<(), HostError> {
    let mut v = ScVal::from(ScVec::default());
    for _ in 0..200 {
        let vv = ScVec::try_from(vec![v])?;
        v = ScVal::from(vv);
    }
    let bytes = v.to_xdr_with_depth_limit(10000)?;
    let res = ScVal::from_xdr_with_depth_limit(bytes, 500).map_err(|e| HostError::from(e));
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}
