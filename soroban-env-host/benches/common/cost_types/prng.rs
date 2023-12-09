use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, SeedableRng};
use rand_chacha::ChaCha20Rng;
use soroban_env_host::{cost_runner::ChaCha20DrawBytesRun, Host};

pub(crate) struct ChaCha20DrawBytesMeasure;

impl HostCostMeasurement for ChaCha20DrawBytesMeasure {
    type Runner = ChaCha20DrawBytesRun;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, input: u64) -> (ChaCha20Rng, Vec<u8>) {
        let size = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        let seed = [0u8; 32];
        let rng = ChaCha20Rng::from_seed(seed);
        let dest = vec![0u8; size as usize];
        (rng, dest)
    }
}
