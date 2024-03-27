use crate::{
    budget::CostTracker,
    cost_runner::{CostRunner, CostType},
    xdr::Hash,
};
use ecdsa::RecoveryId;
use p256::ecdsa::Signature;
use std::hint::black_box;

use super::ExperimentalCostType;

pub struct EcdsaSecp256r1RecoverRun;

#[allow(non_snake_case)]
#[derive(Clone)]
pub struct EcdsaSecp256r1RecoverSample {
    pub msg_hash: Hash,
    pub sig: Signature,
    pub recovery_id: RecoveryId,
}

impl CostRunner for EcdsaSecp256r1RecoverRun {
    const COST_TYPE: CostType = CostType::Experimental(ExperimentalCostType::EcdsaSecp256r1Recover);

    // we want to capture the output variance w.r.t the random input, thus we
    // set `RUN_ITERATIONS` to 1.
    const RUN_ITERATIONS: u64 = 1;

    type SampleType = EcdsaSecp256r1RecoverSample;

    type RecycledType = Self::SampleType;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        black_box(
            host.ecdsa_secp256r1_recover_key(&sample.msg_hash, &sample.sig, sample.recovery_id)
                .unwrap(),
        );
        black_box(sample)
    }

    fn get_tracker(_host: &crate::Host) -> CostTracker {
        CostTracker {
            iterations: Self::RUN_ITERATIONS,
            inputs: None,
            cpu: 0,
            mem: 0,
        }
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(sample)
    }
}
