use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{cost_runner::HostMemCpyRun, Host};

// Measures the cost of copying a chunk of memory in the host (no allocation).
// The input value is the number of bytes copied.
pub(crate) struct HostMemCpyMeasure;

impl HostCostMeasurement for HostMemCpyMeasure {
    type Runner = HostMemCpyRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> ([u8; 32], u64) {
        let mut a: [u8; 32] = [0; 32];
        rng.fill_bytes(a.as_mut_slice());
        (a, input * 1000)
    }
}
