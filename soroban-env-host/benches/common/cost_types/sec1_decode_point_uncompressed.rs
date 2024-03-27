use crate::common::HostCostMeasurement;
use p256::ecdsa::SigningKey;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{
    cost_runner::{Sec1DecodePointSample, Sec1DecodePointUncompressedRun},
    Host,
};

pub(crate) struct Sec1DecodePointUncompressedMeasure {}

impl HostCostMeasurement for Sec1DecodePointUncompressedMeasure {
    type Runner = Sec1DecodePointUncompressedRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Sec1DecodePointSample {
        let mut key_bytes = [0u8; 32];
        rng.fill_bytes(&mut key_bytes);
        let signer = SigningKey::from_bytes(&key_bytes.into()).unwrap();
        let verifying_key = signer.verifying_key();
        let bytes = verifying_key
            .to_encoded_point(false /* compress */)
            .to_bytes();
        Sec1DecodePointSample { bytes }
    }
}
