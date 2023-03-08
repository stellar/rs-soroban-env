use soroban_env_common::{
    xdr::{ScVal, Uint256},
    TryIntoVal, I256,
};

use crate::{Host, HostError, RawVal};

#[test]
fn test_i64_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let i: i64 = -1;
    let rv: RawVal = i.try_into_val(&host)?;
    let iv: i64 = rv.try_into_val(&host)?;
    assert_eq!(i, iv);
    Ok(())
}

#[test]
fn test_i128_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let i: i128 = -1;
    let rv: RawVal = i.try_into_val(&host)?;
    let iv: i128 = rv.try_into_val(&host)?;
    assert_eq!(i, iv);
    Ok(())
}

#[test]
fn test_256_roundtrip() -> Result<(), HostError> {
    let host = Host::default();

    let i: I256 = I256::new(-1234_i128);
    let rv = host.add_host_object(i)?.to_raw();
    let scv_back = host.from_host_val(rv)?;

    let scv_ref = ScVal::I256(Uint256(i.to_be_bytes()));
    assert_eq!(scv_back, scv_ref);

    Ok(())
}
