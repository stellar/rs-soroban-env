use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{cost_runner::MemCpyRun, Host};

// Measures the cost of copying a chunk of memory in the host (no allocation).
// The input value is the number of bytes copied.
pub(crate) struct MemCpyMeasure;

impl HostCostMeasurement for MemCpyMeasure {
    type Runner = MemCpyRun;

    // Rust and LLVM will conspire to optimize the heck out of a large memcpy.
    // This will cause us to gather completely wrong numbers for the cost of a
    // small memcpy, which almost all our memcpys are (they're not even likely
    // to be calls to memcpy, they're just "byte moving in the abstract sense",
    // usually only a few dozen or hundred at a time). So we use the smallest
    // number here we're allowed to use.
    const STEP_SIZE: u64 = 1;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> (Vec<u8>, Vec<u8>) {
        let len = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        let mut a = vec![0; len as usize];
        let mut b = vec![0; len as usize];
        rng.fill_bytes(a.as_mut_slice());
        rng.fill_bytes(b.as_mut_slice());
        (a, b)
    }
}
