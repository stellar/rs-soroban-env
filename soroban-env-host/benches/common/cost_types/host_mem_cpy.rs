use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{cost_runner::HostMemCpyRun, Host};

// Measures the cost of copying a chunk of memory in the host (no allocation).
// The input value is the number of bytes copied.
pub(crate) struct HostMemCpyMeasure;

impl HostCostMeasurement for HostMemCpyMeasure {
    type Runner = HostMemCpyRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> (Vec<u8>, Vec<u8>) {
        let len = (input * 1000) as usize;
        let mut a = vec![0; len];
        let mut b = vec![0; len];
        rng.fill_bytes(a.as_mut_slice());
        rng.fill_bytes(b.as_mut_slice());
        (a, b)
    }
}
