use crate::{Host, HostError};
use soroban_env_common::{Symbol, TryFromVal};

#[test]
fn invalid_chars() -> Result<(), HostError> {
    let host = observe_host!(Host::default());

    let s = "#";
    let s = Symbol::try_from_val(&*host, &s);

    assert!(s.is_err());

    Ok(())
}

#[test]
fn overlong() -> Result<(), HostError> {
    let host = observe_host!(Host::default());

    let s = "123456789012345678901234567890___";
    let s = Symbol::try_from_val(&*host, &s);

    assert!(s.is_err());

    Ok(())
}

#[test]
fn max_len() -> Result<(), HostError> {
    let host = observe_host!(Host::default());

    let s = "123456789012345678901234567890__";
    let s = Symbol::try_from_val(&*host, &s);

    assert!(s.is_ok());

    Ok(())
}

#[test]
fn zero_len() -> Result<(), HostError> {
    let host = observe_host!(Host::default());

    let s = "";
    let s = Symbol::try_from_val(&*host, &s);

    assert!(s.is_ok());

    Ok(())
}
