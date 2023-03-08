use std::convert::TryInto;

use soroban_env_common::{EnvBase, StringObject, TryIntoVal};

use crate::{Env, Host, HostError, RawVal};

#[test]
fn str_conversions() -> Result<(), HostError> {
    let host = Host::default();
    let mut obj = host.bytes_new()?;
    for c in 'a'..='z' {
        obj = host.bytes_push(obj, (c as u32).into())?;
    }
    let ss = "abcdefghijklmnopqrstuvwxyz";
    let so = host.string_new_from_slice(ss)?;
    let raw = so.to_raw();
    let s: String = raw.try_into_val(&host)?;
    assert_eq!(s, ss);

    let raw: RawVal = s.try_into_val(&host)?;
    let obj: StringObject = raw.try_into()?;
    let mut slice: Vec<u8> = vec![0; ss.len()];
    host.string_copy_to_slice(obj, 0_u32.into(), slice.as_mut())?;
    let bytes = host.bytes_new_from_slice(slice.as_slice())?;
    for (i, c) in ('a'..='z').enumerate() {
        let idx = (i as u32).into();
        let c_raw = host.bytes_get(bytes, idx)?;
        let c_u32: u32 = c_raw.try_into()?;
        let c_char: char = char::from_u32(c_u32).unwrap();
        assert_eq!((i, c_char), (i, c));
    }
    Ok(())
}
