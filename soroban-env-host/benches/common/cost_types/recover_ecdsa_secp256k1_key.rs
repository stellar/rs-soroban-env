use crate::common::HostCostMeasurement;
use k256::{ecdsa::SigningKey, SecretKey};
use rand::rngs::StdRng;
use sha3::{Digest, Keccak256};
use soroban_env_host::{
    cost_runner::{RecoverEcdsaSecp256k1KeyRun, RecoverEcdsaSecp256k1KeySample},
    xdr::Hash,
    Host,
};

pub(crate) struct RecoverEcdsaSecp256k1KeyMeasure;

// This measures the cost of verifying an EcdsaSecp256k1 signature of varying-length
// messages. The input value is the length of the signed message. It should cost
// linear CPU (for hashing) and zero heap memory.
impl HostCostMeasurement for RecoverEcdsaSecp256k1KeyMeasure {
    type Runner = RecoverEcdsaSecp256k1KeyRun;

    fn new_random_case(
        _host: &Host,
        _rng: &mut StdRng,
        input: u64,
    ) -> RecoverEcdsaSecp256k1KeySample {
        // Very awkward: the 'rand' crate has two copies linked in due to
        // divergence between the requirements of k256 and ed25519. The StdRng
        // we're getting here is not the one k256 wants. So we use an OsRng
        // here, from the package k256 wants (and re-exports).
        let mut rng = k256::elliptic_curve::rand_core::OsRng;

        let size = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        let sec: SecretKey = SecretKey::random(&mut rng);
        let msg: Vec<u8> = (0..size).map(|x| x as u8).collect();
        let hash: Hash = Hash(Keccak256::digest(msg).into());
        let (sig, rid) = SigningKey::from(sec)
            .sign_prehash_recoverable(hash.as_slice())
            .unwrap();
        RecoverEcdsaSecp256k1KeySample { hash, sig, rid }
    }
}
