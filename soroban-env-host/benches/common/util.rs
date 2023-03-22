use soroban_env_host::RawVal;

pub(crate) const TEST_WASMS: [&'static [u8]; 10] = [
    soroban_test_wasms::ADD_I32,
    soroban_test_wasms::CREATE_CONTRACT,
    soroban_test_wasms::CONTRACT_DATA,
    soroban_test_wasms::LINEAR_MEMORY,
    soroban_test_wasms::VEC,
    soroban_test_wasms::INVOKE_CONTRACT,
    soroban_test_wasms::HOSTILE,
    soroban_test_wasms::FIB,
    soroban_test_wasms::FANNKUCH,
    soroban_test_wasms::COMPLEX,
];

pub(crate) fn to_rawval_u32<I: Iterator<Item = u32>>(vals: I) -> impl Iterator<Item = RawVal> {
    vals.map(move |v| RawVal::from_u32(v).into())
}
