use crate::common::HostCostMeasurement;
use ecdsa::signature::hazmat::PrehashSigner;
use elliptic_curve::scalar::IsHigh;
use k256::{
    ecdsa::{Signature, SigningKey},
    Secp256k1,
};
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{
    cost_runner::{DecodeEcdsaCurve256SigRun, DecodeEcdsaCurve256SigSample},
    Host,
};

pub(crate) struct DecodeEcdsaCurve256SigMeasure;

impl HostCostMeasurement for DecodeEcdsaCurve256SigMeasure {
    type Runner = DecodeEcdsaCurve256SigRun<Secp256k1>;

    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> DecodeEcdsaCurve256SigSample {
        let mut key_bytes = [0u8; 32];
        rng.fill_bytes(&mut key_bytes);
        let signer = SigningKey::from_bytes(&key_bytes.into()).unwrap();
        let mut msg_hash = [0u8; 32];
        rng.fill_bytes(&mut msg_hash);
        let mut sig: Signature = signer.sign_prehash(&msg_hash).unwrap();
        // in our host implementation, we are rejecting high `S`. so here we
        // normalize it to the low S before sending the result
        if bool::from(sig.s().is_high()) {
            sig = sig.normalize_s().unwrap();
        }
        DecodeEcdsaCurve256SigSample {
            bytes: sig.to_vec(),
        }
    }
}
