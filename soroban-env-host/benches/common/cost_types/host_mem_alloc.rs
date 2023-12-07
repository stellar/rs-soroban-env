use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{cost_runner::MemAllocRun, Host};

// Measures the cost of allocating a chunk of memory in the host. The input
// value is the number of bytes allocated.
pub(crate) struct MemAllocMeasure;

impl HostCostMeasurement for MemAllocMeasure {
    type Runner = MemAllocRun;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, input: u64) -> u64 {
        // we just pass along the size and let the runner allocate the memory
        // of the given size
        Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE
    }
}
