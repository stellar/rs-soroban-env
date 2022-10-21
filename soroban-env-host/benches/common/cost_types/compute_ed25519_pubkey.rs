use crate::common::HostCostMeasurement;
use ed25519_dalek::{PublicKey, SecretKey};
use rand::rngs::StdRng;
use soroban_env_host::{budget::CostType, Host};

// This measures the costs to turn one byte buffer into an Ed25519
// pubkey, which should be constant time. The input value is ignored.
pub(crate) struct ComputeEd25519PubKeyRun {
    key: Vec<u8>,
}

impl HostCostMeasurement for ComputeEd25519PubKeyRun {
    const COST_TYPE: CostType = CostType::ComputeEd25519PubKey;
    const RUN_ITERATIONS: u64 = 100;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Self {
        let secret = SecretKey::generate(rng);
        let public: PublicKey = (&secret).into();
        let key = public.as_bytes().as_slice().into();
        Self { key }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        1
    }

    fn run(&mut self, _iter: u64, _host: &Host) {
        ed25519_dalek::PublicKey::from_bytes(self.key.as_slice()).expect("publickey");
    }
}
