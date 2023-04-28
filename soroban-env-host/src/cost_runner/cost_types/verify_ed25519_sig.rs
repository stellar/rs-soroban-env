use std::hint::black_box;

use crate::{cost_runner::CostRunner, xdr::ContractCostType};
use ed25519_dalek::{PublicKey, Signature};

pub struct VerifyEd25519SigRun;

#[derive(Clone)]
pub struct VerifyEd25519SigSample {
    pub key: PublicKey,
    pub msg: Vec<u8>,
    pub sig: Signature,
}

impl CostRunner for VerifyEd25519SigRun {
    const COST_TYPE: ContractCostType = ContractCostType::VerifyEd25519Sig;

    type SampleType = VerifyEd25519SigSample;

    type RecycledType = Self::SampleType;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        black_box(
            host.verify_sig_ed25519_internal(sample.msg.as_slice(), &sample.key, &sample.sig)
                .expect("verify sig ed25519"),
        );
        sample
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, Some(0)).unwrap());
        black_box(sample)
    }
}
