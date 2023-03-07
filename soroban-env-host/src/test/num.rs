use soroban_env_common::TryIntoVal;

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
