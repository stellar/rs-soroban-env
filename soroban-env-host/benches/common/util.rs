use im_rc::{OrdMap, Vector};
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{EnvVal, Host, RawVal};

type HostVal = EnvVal<Host, RawVal>;

pub(crate) fn to_envval_u32<I: Iterator<Item = u32>>(
    host: &Host,
    vals: I,
) -> impl Iterator<Item = HostVal> {
    let host = host.clone();
    vals.map(move |v| EnvVal {
        env: host.clone(),
        val: RawVal::from_u32(v),
    })
}

pub(crate) fn random_ord_map(
    host: &Host,
    rng: &mut StdRng,
    input: u64,
) -> OrdMap<HostVal, HostVal> {
    let mut kv: Vec<(HostVal, HostVal)> = Vec::with_capacity(input as usize);
    for _i in 0..input {
        let k = EnvVal {
            env: host.clone(),
            val: RawVal::from_u32(rng.next_u32()),
        };
        let v = EnvVal {
            env: host.clone(),
            val: RawVal::from_u32(rng.next_u32()),
        };
        kv.push((k, v));
    }
    kv.into()
}

pub(crate) fn random_im_vector(host: &Host, rng: &mut StdRng, input: u64) -> Vector<HostVal> {
    let mut res: Vector<HostVal> = Vector::new();
    for _i in 0..input {
        let v = EnvVal {
            env: host.clone(),
            val: RawVal::from_u32(rng.next_u32()),
        };
        res.push_back(v)
    }
    res
}
