use crate::common::HostCostMeasurement;
use elliptic_curve::scalar::IsHigh;
use p256::ecdsa::SigningKey;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{
    cost_runner::{EcdsaSecp256r1RecoverRun, EcdsaSecp256r1RecoverSample},
    xdr::Hash,
    Host,
};

pub(crate) struct EcdsaSecp256r1RecoverMeasure {}

impl HostCostMeasurement for EcdsaSecp256r1RecoverMeasure {
    type Runner = EcdsaSecp256r1RecoverRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> EcdsaSecp256r1RecoverSample {
        let mut key_bytes = [0u8; 32];
        rng.fill_bytes(&mut key_bytes);
        let signer = SigningKey::from_bytes(&key_bytes.into()).unwrap();
        let mut msg_hash = [0u8; 32];
        rng.fill_bytes(&mut msg_hash);
        let (mut sig, recovery_id) = signer.sign_prehash_recoverable(&msg_hash).unwrap();
        // in our host implementation, we are rejecting high `s`, we are doing it here too.
        if bool::from(sig.s().is_high()) {
            sig = sig.normalize_s().unwrap()
        }
        EcdsaSecp256r1RecoverSample {
            msg_hash: Hash::from(msg_hash),
            sig,
            recovery_id,
        }
    }
}
