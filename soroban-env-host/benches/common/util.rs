use soroban_env_host::{Host, RawVal};

pub(crate) fn to_rawval_u32<I: Iterator<Item = u32>>(
    _host: &Host,
    vals: I,
) -> impl Iterator<Item = RawVal> {
    vals.map(|v| RawVal::from_u32(v))
}
