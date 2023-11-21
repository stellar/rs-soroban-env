#![no_std]
use soroban_sdk::{contract, contractimpl, Bytes, Env};
use soroban_env_common::{VecObject, MapObject, Val, EnvBase, Error};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    // Produce a bytes with 1 byte from each 8 bits of a 32-bit word.
    pub fn bin_word(e: Env, word: u32) -> Bytes {
        let buf: [u8; 4] = [
            (word >> 24) as u8,
            (word >> 16) as u8,
            (word >> 8) as u8,
            word as u8,
        ];
        Bytes::from_slice(&e, &buf)
    }

    // Transform a 4-byte bytes into a new one with each byte incremented,
    // via a temporary guest memory buffer.
    pub fn bin_inc(e: Env, bytes: Bytes) -> Bytes {
        let mut buf: [u8; 4] = [0; 4];
        bytes.copy_into_slice(&mut buf);
        for i in buf.iter_mut() {
            *i += 1;
        }
        Bytes::from_slice(&e, &buf)
    }

    // Bounce a vector of vals off the host, successfully
    pub fn vec_mem_ok(e: Env) -> Result<(), Error> {
        let in_buf: [Val; 3] = [Val::from(1), Val::from(2), Val::from(3)];
        let mut out_buf: [Val; 3] = [Val::from(0); 3];
        let vec: VecObject = e.vec_new_from_slice(&in_buf)?;
        e.vec_unpack_to_slice(vec, &mut out_buf)?;
        assert!(in_buf[0].shallow_eq(&out_buf[0]));
        assert!(in_buf[1].shallow_eq(&out_buf[1]));
        assert!(in_buf[2].shallow_eq(&out_buf[2]));
        Ok(())
    }

    // Same but with a length mismatch
    pub fn vec_mem_bad(e: Env) -> Result<(), Error> {
        let in_buf: [Val; 3] = [Val::from(1), Val::from(2), Val::from(3)];
        let mut long_buf: [Val; 4] = [Val::from(0); 4];
        let mut short_buf: [Val; 2] = [Val::from(0); 2];
        let vec: VecObject = e.vec_new_from_slice(&in_buf)?;
        assert!(e.vec_unpack_to_slice(vec, &mut short_buf).is_err());
        assert!(e.vec_unpack_to_slice(vec, &mut long_buf).is_err());
        Ok(())
    }

    // Bounce a map of vals off the host, successfully
    pub fn map_mem_ok(e: Env) -> Result<(), Error> {
        let map: MapObject = e.map_new_from_slices(&["a", "b"], &[1u32.into(), 2u32.into()])?;

        let key_buf = ["a", "b"];
        let mut val_buf: [Val; 2] = [Val::from(0); 2];

        e.map_unpack_to_slice(map, &key_buf, &mut val_buf)?;
        Ok(())
    }

    // Same but with out of order keys
    pub fn map_mem_bad(e: Env) -> Result<(), Error> {
        // out of order
        assert!(e.map_new_from_slices(&["b", "a"], &[1u32.into(), 2u32.into()]).is_err());

        // duplicate
        assert!(e.map_new_from_slices(&["a", "a"], &[1u32.into(), 2u32.into()]).is_err());

        Ok(())
    }
}
