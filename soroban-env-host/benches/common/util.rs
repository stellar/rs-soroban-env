use num_bigint::{BigInt, Sign};
use rand::{rngs::StdRng, Rng};
use soroban_env_host::{EnvVal, Host, RawVal};

pub(crate) fn to_envval_u32<I: Iterator<Item = u32>>(
    host: &Host,
    vals: I,
) -> impl Iterator<Item = EnvVal<Host, RawVal>> {
    let host = host.clone();
    vals.map(move |v| EnvVal {
        env: host.clone(),
        val: RawVal::from_u32(v),
    })
}

pub(crate) fn repeating_byte_bigint(byte: u8, input: u64) -> BigInt {
    let buf: Vec<u8> = (0..input / 8).map(|_| byte).collect();
    let a = BigInt::from_bytes_be(Sign::Plus, &buf);
    let one: BigInt = 1.into();
    a + one
}

pub(crate) fn random_bigint(rng: &mut StdRng, input: u64) -> BigInt {
    let buf: Vec<u8> = (0..input / 8).map(|_| rng.gen::<u8>()).collect();
    let a = BigInt::from_bytes_be(Sign::Plus, &buf);
    let one: BigInt = 1.into();
    a + one
}
