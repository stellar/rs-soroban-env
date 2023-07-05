use soroban_env_host::{budget::AsBudget, Host, Val, I256};

pub(crate) fn test_host() -> Host {
    let host = Host::default();
    host.as_budget().reset_unlimited().unwrap();
    host.as_budget().reset_fuel_config().unwrap();
    host
}

pub(crate) const TEST_WASMS: [&'static [u8]; 10] = [
    soroban_test_wasms::ADD_I32,
    soroban_test_wasms::CREATE_CONTRACT,
    soroban_test_wasms::CONTRACT_STORAGE,
    soroban_test_wasms::LINEAR_MEMORY,
    soroban_test_wasms::VEC,
    soroban_test_wasms::INVOKE_CONTRACT,
    soroban_test_wasms::HOSTILE,
    soroban_test_wasms::FIB,
    soroban_test_wasms::FANNKUCH,
    soroban_test_wasms::COMPLEX,
];

pub(crate) fn to_rawval_u32<I: Iterator<Item = u32>>(vals: I) -> impl Iterator<Item = Val> {
    vals.map(move |v| Val::from_u32(v).into())
}

pub(crate) fn repeating_byte_i256(byte: u8, input: u64) -> I256 {
    let buf: Vec<u8> = (0..input.min(32)).map(|_| byte).collect();
    let mut res = vec![0u8; 32 - input as usize];
    res.extend_from_slice(buf.as_slice());
    I256::from_be_bytes(res.try_into().unwrap())
}
