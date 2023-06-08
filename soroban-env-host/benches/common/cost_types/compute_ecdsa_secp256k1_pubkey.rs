use crate::common::HostCostMeasurement;
use k256::{PublicKey, SecretKey};
use rand::rngs::StdRng;
use soroban_env_host::{cost_runner::ComputeEcdsaSecp256k1PubKeyRun, Host};

// This measures the costs to turn one byte buffer into an EcdsaSecp256k1
// pubkey, which should be constant time. The input value is ignored.
pub(crate) struct ComputeEcdsaSecp256k1PubKeyMeasure {
    key: Vec<u8>,
}

impl HostCostMeasurement for ComputeEcdsaSecp256k1PubKeyMeasure {
    type Runner = ComputeEcdsaSecp256k1PubKeyRun;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, _input: u64) -> Vec<u8> {
        // Very awkward: the 'rand' crate has two copies linked in due to
        // divergence between the requirements of k256 and ed25519. The StdRng
        // we're getting here is not the one k256 wants. So we use an OsRng
        // here, from the package k256 wants (and re-exports).
        let mut rng = k256::elliptic_curve::rand_core::OsRng;

        let secret = SecretKey::random(&mut rng);
        let public: PublicKey = secret.public_key();
        public.to_sec1_bytes().into_vec()
    }
}
