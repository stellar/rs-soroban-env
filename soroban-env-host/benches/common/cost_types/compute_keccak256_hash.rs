use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{cost_runner::ComputeKeccak256HashRun, Host};

// This measures the costs of performing a keccak256 hash on a variable-sized
// byte buffer. The input value is the size of the buffer. It should be
// linear time.
pub(crate) struct ComputeKeccak256HashMeasure;

impl HostCostMeasurement for ComputeKeccak256HashMeasure {
    type Runner = ComputeKeccak256HashRun;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, input: u64) -> Vec<u8> {
        let size = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        (0..size).map(|n| n as u8).collect()
    }
}
