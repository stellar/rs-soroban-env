use std::convert::TryInto;

use soroban_env_common::{EnvBase, StringObject, TryIntoVal};

use crate::{Env, Host, HostError, Val};

#[test]
fn str_conversions() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let mut obj = host.bytes_new()?;
    for c in 'a'..='z' {
        obj = host.bytes_push(obj, (c as u32).into())?;
    }
    let ss = "abcdefghijklmnopqrstuvwxyz";
    let so = host.string_new_from_slice(ss.as_bytes())?;
    let val = so.to_val();
    let s: String = val.try_into_val(&*host)?;
    assert_eq!(s, ss);

    let val: Val = s.try_into_val(&*host)?;
    let obj: StringObject = val.try_into()?;
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

#[test]
fn string_bytes_roundtrip() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let s = "abcdefghijklmnopqrstuvwxyz";
    let so = host.string_new_from_slice(s.as_bytes())?;
    let bytes_obj = host.string_to_bytes(so)?;
    // Check that the bytes match the original string
    let mut buf = vec![0u8; s.len()];
    host.bytes_copy_to_slice(bytes_obj, 0_u32.into(), buf.as_mut())?;
    assert_eq!(buf, s.as_bytes());

    // Now convert back
    let so2 = host.bytes_to_string(bytes_obj)?;
    let mut buf2 = vec![0u8; s.len()];
    host.string_copy_to_slice(so2, 0_u32.into(), buf2.as_mut())?;
    assert_eq!(buf2, s.as_bytes());
    Ok(())
}

#[test]
fn string_ops_via_bytes() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let s = "abcdefghijklmnopqrstuvwxyz";
    let bytes_obj = host.bytes_new_from_slice(s.as_bytes())?;
    let mut obj = host.bytes_to_string(bytes_obj)?;
    let mut buf = vec![0u8; s.len()];
    host.string_copy_to_slice(obj, 0_u32.into(), buf.as_mut())?;
    assert_eq!(buf, s.as_bytes());

    // pop and len (via bytes)
    let mut bytes = host.string_to_bytes(obj)?;
    for _ in 0..24 {
        bytes = host.bytes_pop(bytes)?;
    }
    obj = host.bytes_to_string(bytes)?;
    assert_eq!(u32::from(host.string_len(obj)?), 2_u32); // "ab"

    // get, put, del, front, back (via bytes)
    let mut bytes = host.string_to_bytes(obj)?;
    bytes = host.bytes_put(bytes, 1_u32.into(), 99_u32.into())?; // "ac"
    obj = host.bytes_to_string(bytes)?;
    let mut buf = vec![0u8; 2];
    host.string_copy_to_slice(obj, 0_u32.into(), buf.as_mut())?;
    assert_eq!(buf, b"ac");
    let mut bytes = host.string_to_bytes(obj)?;
    bytes = host.bytes_del(bytes, 1_u32.into())?;
    obj = host.bytes_to_string(bytes)?;
    assert_eq!(u32::from(host.string_len(obj)?), 1_u32);
    let mut buf = vec![0u8; 1];
    host.string_copy_to_slice(obj, 0_u32.into(), buf.as_mut())?;
    assert_eq!(buf[0], b'a');

    // insert, slice, append (via bytes)
    let mut bytes = host.string_to_bytes(obj)?;
    bytes = host.bytes_insert(bytes, 1_u32.into(), (b'b' as u32).into())?;
    obj = host.bytes_to_string(bytes)?;
    let mut buf = vec![0u8; 2];
    host.string_copy_to_slice(obj, 0_u32.into(), buf.as_mut())?;
    assert_eq!(buf, b"ab");

    // slice
    let mut bytes = host.bytes_new_from_slice(s.as_bytes())?;
    bytes = host.bytes_slice(bytes, 5u32.into(), 10u32.into())?;
    let sliced_str = host.bytes_to_string(bytes)?;
    let mut buf = vec![0u8; 5];
    host.string_copy_to_slice(sliced_str, 0_u32.into(), buf.as_mut())?;
    assert_eq!(buf, b"fghij");

    // append
    let str1 = host.string_new_from_slice(b"abcde")?;
    let str2 = host.string_new_from_slice(b"fghij")?;
    let bytes1 = host.string_to_bytes(str1)?;
    let bytes2 = host.string_to_bytes(str2)?;
    let appended_bytes = host.bytes_append(bytes1, bytes2)?;
    let appended_str = host.bytes_to_string(appended_bytes)?;
    let mut buf = vec![0u8; 10];
    host.string_copy_to_slice(appended_str, 0_u32.into(), buf.as_mut())?;
    assert_eq!(buf, b"abcdefghij");

    Ok(())
}
