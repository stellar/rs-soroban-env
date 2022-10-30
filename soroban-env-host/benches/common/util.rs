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
