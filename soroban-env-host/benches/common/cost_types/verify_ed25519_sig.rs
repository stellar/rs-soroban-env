use crate::common::HostCostMeasurement;
use ed25519_dalek::{Keypair, PublicKey, Signature, Signer, Verifier};
use rand::rngs::StdRng;
use soroban_env_host::{budget::CostType, Host};

pub(crate) struct VerifyEd25519SigRun {
    key: PublicKey,
    msg: Vec<u8>,
    sig: Signature,
}

// This measures the cost of verifying an Ed25519 signature of varying-length
// messages. The input value is the length of the signed message. It should cost
// linear CPU (for hashing) and zero heap memory.
impl HostCostMeasurement for VerifyEd25519SigRun {
    const COST_TYPE: CostType = CostType::VerifyEd25519Sig;
    const RUN_ITERATIONS: u64 = 3;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Self {
        let size = input * 10000;
        let keypair: Keypair = Keypair::generate(rng);
        let key: PublicKey = keypair.public.clone();
        let msg: Vec<u8> = (0..size).map(|x| x as u8).collect();
        let sig: Signature = keypair.sign(msg.as_slice());
        Self { key, msg, sig }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.msg.len() as u64
    }

    fn run(&mut self, _iter: u64, _host: &Host) {
        self.key
            .verify(self.msg.as_slice(), &self.sig)
            .expect("verify")
    }
}
