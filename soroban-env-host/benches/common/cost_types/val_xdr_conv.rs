use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{cost_runner::ValXdrConvRun, xdr::ScVal, Host, RawVal};

pub(crate) struct ValXdrConvMeasure;

// This measures the costs of conversion of one non-object RawVal type to and from XDR.
impl HostCostMeasurement for ValXdrConvMeasure {
    type Runner = ValXdrConvRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> (RawVal, ScVal) {
        let v = rng.next_u32();
        let rv: RawVal = v.into();
        let scv: ScVal = ScVal::U32(v as u32);
        (rv, scv)
    }
}
