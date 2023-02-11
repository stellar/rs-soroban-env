use soroban_env_common::{RawVal, TryIntoVal, Void};
use soroban_env_host::{Host, HostError};

#[test]
fn some() -> Result<(), HostError> {
    let host = Host::default();

    let some = Some(1u32);
    let val: RawVal = some.try_into_val(&host)?;
    assert!(val.is::<u32>());
    let u32: u32 = val.try_into_val(&host)?;
    assert_eq!(u32, 1);

    assert_eq!(some, val.try_into_val(&host)?);
    Ok(())
}

#[test]
fn none() -> Result<(), HostError> {
    let host = Host::default();

    let none: Option<u32> = None;
    let val: RawVal = none.try_into_val(&host)?;
    assert!(val.is::<Void>());

    assert_eq!(none, val.try_into_val(&host)?);
    Ok(())
}
