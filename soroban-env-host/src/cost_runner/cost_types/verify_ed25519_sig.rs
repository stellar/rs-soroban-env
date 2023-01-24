use crate::{budget::CostType, cost_runner::CostRunner};
use ed25519_dalek::{PublicKey, Signature};

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

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        host.verify_sig_ed25519_internal(sample.msg.as_slice(), &sample.key, &sample.sig)
            .expect("verify sig ed25519");
    }
}
