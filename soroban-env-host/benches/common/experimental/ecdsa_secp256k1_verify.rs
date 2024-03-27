use crate::common::HostCostMeasurement;
use ecdsa::signature::hazmat::PrehashSigner;
use elliptic_curve::scalar::IsHigh;
use k256::ecdsa::{Signature, SigningKey};
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{
    cost_runner::{EcdsaSecp256k1VerifyRun, EcdsaSecp256k1VerifySample},
    xdr::Hash,
    Host,
};

pub(crate) struct EcdsaSecp256k1VerifyMeasure {}

impl HostCostMeasurement for EcdsaSecp256k1VerifyMeasure {
    type Runner = EcdsaSecp256k1VerifyRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> EcdsaSecp256k1VerifySample {
        let mut key_bytes = [0u8; 32];
        rng.fill_bytes(&mut key_bytes);
        let signer = SigningKey::from_bytes(&key_bytes.into()).unwrap();
        let mut msg_hash = [0u8; 32];
        rng.fill_bytes(&mut msg_hash);
        let mut sig: Signature = signer.sign_prehash(&msg_hash).unwrap();
        // in our host implementation, we are rejecting high `s`, we are doing it here too.
        if bool::from(sig.s().is_high()) {
            sig = sig.normalize_s().unwrap()
        }
        EcdsaSecp256k1VerifySample {
            pub_key: signer.verifying_key().clone(),
            msg_hash: Hash::from(msg_hash),
            sig,
        }
    }
}
