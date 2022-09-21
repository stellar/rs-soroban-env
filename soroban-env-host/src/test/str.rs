use std::convert::TryInto;

use soroban_env_common::TryIntoVal;

use crate::{CheckedEnv, Host, HostError, Object, RawVal};

#[test]
fn str_conversions() -> Result<(), HostError> {
    let host = Host::default();
    let mut obj = host.bytes_new()?;
    for c in 'a'..='z' {
        obj = host.bytes_push(obj, (c as u32).into())?;
    }
    let raw = obj.to_raw();
    let s: String = raw.try_into_val(&host)?;
    assert_eq!(s, "abcdefghijklmnopqrstuvwxyz");
    let raw: RawVal = s.try_into_val(&host)?;
    let obj: Object = raw.try_into_val(&host)?;
    for (i, c) in ('a'..='z').enumerate() {
        let c_raw = host.bytes_get(obj, (i as u32).into())?;
        let c_u32: u32 = c_raw.try_into()?;
        let c_char: char = char::from_u32(c_u32).unwrap();
        assert_eq!((i, c_char), (i, c));
    }
    Ok(())
}
