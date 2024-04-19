use crate::{
    budget::CostTracker,
    cost_runner::{CostRunner, CostType, DecodeEcdsaCurve256SigRun, DecodeEcdsaCurve256SigSample},
};
use p256::NistP256;

use super::ExperimentalCostType;

// This experiment verifies that decoding an ECDSA signature with the underlying
// curve being secp256r1 yield similar costs as secp256k1 (currently in use)

pub struct DecodeSecp256r1SigRun;

impl CostRunner for DecodeSecp256r1SigRun {
    const COST_TYPE: CostType =
        CostType::Experimental(ExperimentalCostType::DecodeSecp256r1Signature);

    // we want to capture the output variance w.r.t the random input, thus we
    // set `RUN_ITERATIONS` to 1.
    const RUN_ITERATIONS: u64 = 1;

    type SampleType = DecodeEcdsaCurve256SigSample;

    type RecycledType = <DecodeEcdsaCurve256SigRun<NistP256> as CostRunner>::RecycledType;

    fn run_iter(host: &crate::Host, iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        DecodeEcdsaCurve256SigRun::<NistP256>::run_iter(host, iter, sample)
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
        host: &crate::Host,
        iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        DecodeEcdsaCurve256SigRun::run_baseline_iter(host, iter, sample)
    }
}
