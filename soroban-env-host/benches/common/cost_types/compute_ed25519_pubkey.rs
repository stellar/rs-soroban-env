use crate::common::HostCostMeasurement;
use ed25519_dalek::SigningKey;
use rand::rngs::StdRng;
use soroban_env_host::{cost_runner::ComputeEd25519PubKeyRun, Host};

// This measures the costs to turn one byte buffer into an Ed25519
// pubkey, which should be constant time. The input value is ignored.
pub(crate) struct ComputeEd25519PubKeyMeasure {
    key: Vec<u8>,
}

impl HostCostMeasurement for ComputeEd25519PubKeyMeasure {
    type Runner = ComputeEd25519PubKeyRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Vec<u8> {
        let key = SigningKey::generate(rng);
        key.verifying_key().to_bytes().into()
    }
}
