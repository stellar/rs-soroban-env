use crate::{
    cost_runner::{CostRunner, CostType},
    xdr::{ContractCostType::VerifyEcdsaSecp256r1Sig, Hash},
};
use p256::ecdsa::{Signature, VerifyingKey};
use std::hint::black_box;
pub struct VerifyEcdsaSecp256r1SigRun;

#[derive(Clone)]
pub struct VerifyEcdsaSecp256r1SigSample {
    pub pub_key: VerifyingKey,
    pub msg_hash: Hash,
    pub sig: Signature,
}

impl CostRunner for VerifyEcdsaSecp256r1SigRun {
    const COST_TYPE: CostType = CostType::Contract(VerifyEcdsaSecp256r1Sig);

    type SampleType = VerifyEcdsaSecp256r1SigSample;

    type RecycledType = Self::SampleType;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        black_box(
            host.secp256r1_verify_signature(&sample.pub_key, &sample.msg_hash, &sample.sig)
                .unwrap(),
        );
        black_box(sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(VerifyEcdsaSecp256r1Sig, None).unwrap());
        black_box(sample)
    }
}
