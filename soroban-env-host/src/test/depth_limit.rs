use soroban_env_common::xdr::{Limits, ReadXdr, WriteXdr};

use crate::{
    budget::AsBudget,
    host::metered_clone::MeteredClone,
    xdr::{ScErrorCode, ScErrorType, ScVal, ScVec},
    Env, Host, HostError, DEFAULT_XDR_RW_LIMITS,
};

#[test]
fn deep_scval_to_host_val() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
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
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn deep_host_val_to_scval() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.as_budget().reset_unlimited()?;

    let mut hv = host.test_vec_obj::<u32>(&[])?;
    for _ in 0..1000 {
        let vv = host.test_vec_obj::<u32>(&[])?;
        hv = host.vec_push_back(vv, hv.to_val())?;
    }
    let res = host.from_host_obj(hv);
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn deep_host_obj_clone() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
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
    let host = observe_host!(Host::test_host());
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
fn depth_limited_scval_xdr_serialization() -> Result<(), HostError> {
    let mut v = ScVal::from(ScVec::default());
    for _ in 0..200 {
        let vv = ScVec::try_from(vec![v])?;
        v = ScVal::from(vv);
    }
    // default depth limit will cause serialization to fail
    let res = v
        .to_xdr(DEFAULT_XDR_RW_LIMITS)
        .map_err(|e| HostError::from(e));
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));

    // remove the Limits to make serialization pass
    let bytes = v.to_xdr(Limits::none())?;
    // deserialization from the input with limits will fail
    let res = ScVal::from_xdr(bytes, DEFAULT_XDR_RW_LIMITS).map_err(|e| HostError::from(e));
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn length_limited_scval_xdr_conversion() -> Result<(), HostError> {
    let buf = vec![0; 400000];
    let scv_bytes = ScVal::Bytes(buf.try_into().unwrap());
    let mut scv_vec = ScVal::Vec(Some(ScVec(vec![scv_bytes.clone(); 1].try_into().unwrap())));
    // roughly consumes 40MiB (> 32 MiB the limit)
    for _i in 1..100 {
        let mut v = vec![scv_vec; 1];
        v.push(scv_bytes.clone());
        scv_vec = ScVal::Vec(Some(v.try_into().unwrap()));
    }
    // default length limit will cause serialization to fail
    let res = scv_vec
        .to_xdr(DEFAULT_XDR_RW_LIMITS)
        .map_err(|e| HostError::from(e));
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));

    // remove the Limits to make serialization pass
    let bytes = scv_vec
        .to_xdr(Limits::none())
        .map_err(|e| HostError::from(e))?;
    // deserialization from the input with limits will fail
    let res = ScVal::from_xdr(bytes, DEFAULT_XDR_RW_LIMITS).map_err(|e| HostError::from(e));
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}
