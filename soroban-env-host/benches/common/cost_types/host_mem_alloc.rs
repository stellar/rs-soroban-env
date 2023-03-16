use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{cost_runner::HostMemAllocRun, Host};

// Measures the cost of allocating a chunk of memory in the host. The input
// value is the number of bytes allocated.
pub(crate) struct HostMemAllocMeasure;

impl HostCostMeasurement for HostMemAllocMeasure {
    type Runner = HostMemAllocRun;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, input: u64) -> u64 {
        // we just pass along the size and let the runner allocate the memory
        // of the given size
        input * 1000
    }
}
