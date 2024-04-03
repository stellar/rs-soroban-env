use crate::common::HostCostMeasurement;
use ecdsa::signature::hazmat::PrehashSigner;
use elliptic_curve::scalar::IsHigh;
use p256::ecdsa::{Signature, SigningKey};
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{
    cost_runner::{VerifyEcdsaSecp256r1SigRun, VerifyEcdsaSecp256r1SigSample},
    xdr::Hash,
    Host,
};

pub(crate) struct VerifyEcdsaSecp256r1SigMeasure {}

impl HostCostMeasurement for VerifyEcdsaSecp256r1SigMeasure {
    type Runner = VerifyEcdsaSecp256r1SigRun;

    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> VerifyEcdsaSecp256r1SigSample {
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
        VerifyEcdsaSecp256r1SigSample {
            pub_key: signer.verifying_key().clone(),
            msg_hash: Hash::from(msg_hash),
            sig,
        }
    }
}
