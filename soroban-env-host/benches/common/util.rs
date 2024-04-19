use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{budget::AsBudget, meta, Host, LedgerInfo, Val, I256};

pub(crate) fn test_host() -> Host {
    let host = Host::default();
    host.set_ledger_info(LedgerInfo {
        protocol_version: Host::current_test_protocol(),
        ..Default::default()
    })
    .unwrap();
    host.as_budget().reset_unlimited().unwrap();
    host.as_budget().reset_fuel_config().unwrap();
    if std::env::var("DEBUG_BENCH_HOST").is_ok() {
        host.enable_debug().unwrap();
    }
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

pub(crate) fn u32_iter_to_val_iter<I: Iterator<Item = u32>>(vals: I) -> impl Iterator<Item = Val> {
    vals.map(move |v| Val::from_u32(v).into())
}

pub(crate) fn repeating_byte_i256(byte: u8, input: u64) -> I256 {
    let buf: Vec<u8> = (0..input.min(32)).map(|_| byte).collect();
    let mut res = vec![0u8; 32 - input as usize];
    res.extend_from_slice(buf.as_slice());
    I256::from_be_bytes(res.try_into().unwrap())
}

pub(crate) fn randvec(rng: &mut StdRng, len: u64) -> Vec<u8> {
    let mut res: Vec<u8> = vec![0; len as usize];
    rng.fill_bytes(res.as_mut_slice());
    res
}
