use crate::{budget::CostType, cost_runner::CostRunner};
use ed25519_dalek::{PublicKey, Signature, Verifier};

pub struct VerifyEd25519SigRun;

#[derive(Clone)]
pub struct VerifyEd25519SigSample {
    pub key: PublicKey,
    pub msg: Vec<u8>,
    pub sig: Signature,
}

impl CostRunner for VerifyEd25519SigRun {
    const COST_TYPE: CostType = CostType::VerifyEd25519Sig;
    type SampleType = VerifyEd25519SigSample;

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample
            .key
            .verify(sample.msg.as_slice(), &sample.sig)
            .expect("verify");
    }
}
