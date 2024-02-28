use crate::common::HostCostMeasurement;
use k256::{
    ecdsa::{signature::Signer, Signature, SigningKey},
    SecretKey,
};
use rand::rngs::StdRng;
use soroban_env_host::{cost_runner::ComputeEcdsaSecp256k1SigRun, Host};

// This measures the costs to turn one byte buffer into an EcdsaSecp256k1
// signature, which should be constant time. The input value is ignored.
pub(crate) struct ComputeEcdsaSecp256k1SigMeasure {
    sig: Vec<u8>,
}

impl HostCostMeasurement for ComputeEcdsaSecp256k1SigMeasure {
    type Runner = ComputeEcdsaSecp256k1SigRun;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, input: u64) -> Vec<u8> {
        let size = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;

        // Very awkward: the 'rand' crate has two copies linked in due to
        // divergence between the requirements of k256 and ed25519. The StdRng
        // we're getting here is not the one k256 wants. So we use an OsRng
        // here, from the package k256 wants (and re-exports).
        let mut rng = k256::elliptic_curve::rand_core::OsRng;

        let sec: SecretKey = SecretKey::random(&mut rng);
        let msg: Vec<u8> = (0..size).map(|x| x as u8).collect();
        let sig: Signature = SigningKey::from(sec).try_sign(msg.as_slice()).unwrap();
        sig.to_bytes().to_vec()
    }
}
