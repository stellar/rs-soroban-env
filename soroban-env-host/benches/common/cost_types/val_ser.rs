use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{cost_runner::ValSerRun, xdr::ScVal, Host};

pub(crate) struct ValSerMeasure;

// This measures the costs of converting an ScVal into bytes.
impl HostCostMeasurement for ValSerMeasure {
    type Runner = ValSerRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> (ScVal, Vec<u8>) {
        let len = 1 + input * Self::STEP_SIZE;
        let mut buf = vec![0; len as usize];
        rng.fill_bytes(buf.as_mut_slice());
        let v = ScVal::Bytes(buf.try_into().unwrap());
        (v, Vec::default())
    }
}
