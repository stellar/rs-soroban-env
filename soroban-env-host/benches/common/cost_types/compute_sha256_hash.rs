use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use sha2::{Digest, Sha256};
use soroban_env_host::{budget::CostType, Host};

// This measures the costs of performing a sha256 hash on a variable-sized
// byte buffer. The input value is the size of the buffer. It should be
// linear time.
pub(crate) struct ComputeSha256HashRun {
    buf: Vec<u8>,
}

impl HostCostMeasurement for ComputeSha256HashRun {
    const COST_TYPE: CostType = CostType::ComputeSha256Hash;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, input: u64) -> Self {
        let size = input * 100;
        let buf: Vec<u8> = (0..size).map(|n| n as u8).collect();
        Self { buf }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.buf.len() as u64
    }

    fn run(&mut self, _iter: u64, _host: &Host) {
        Sha256::digest(&self.buf).as_slice().to_vec();
    }
}
