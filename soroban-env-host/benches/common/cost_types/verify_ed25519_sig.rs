use crate::common::HostCostMeasurement;
use ed25519_dalek::{Keypair, PublicKey, Signature, Signer};
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::{VerifyEd25519SigRun, VerifyEd25519SigSample},
    Host,
};

pub(crate) struct VerifyEd25519SigMeasure;

// This measures the cost of verifying an Ed25519 signature of varying-length
// messages. The input value is the length of the signed message. It should cost
// linear CPU (for hashing) and zero heap memory.
impl HostCostMeasurement for VerifyEd25519SigMeasure {
    type Runner = VerifyEd25519SigRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> VerifyEd25519SigSample {
        let size = input * 10000;
        let keypair: Keypair = Keypair::generate(rng);
        let key: PublicKey = keypair.public.clone();
        let msg: Vec<u8> = (0..size).map(|x| x as u8).collect();
        let sig: Signature = keypair.sign(msg.as_slice());
        VerifyEd25519SigSample { key, msg, sig }
    }
}
