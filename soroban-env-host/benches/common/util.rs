use soroban_env_host::RawVal;

pub(crate) fn to_rawval_u32<I: Iterator<Item = u32>>(vals: I) -> impl Iterator<Item = RawVal> {
    vals.map(move |v| RawVal::from_u32(v))
}
