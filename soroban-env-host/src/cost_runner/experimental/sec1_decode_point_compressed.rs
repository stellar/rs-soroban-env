use crate::{
    budget::CostTracker,
    cost_runner::{CostRunner, CostType, Sec1DecodePointSample, Sec1DecodePointUncompressedRun},
};
use p256::ecdsa::VerifyingKey;

use super::ExperimentalCostType;

// This is almost idential to `Sec1DecodePointUncompressedRun` -- the one we are
// officially using, most of methods here are just pass through to its impl. The
// difference in compression methods are in handled in the measurement side, by
// specifing compression method during sample gernation. The only difference
// here is 1. use the experimental cost type 2. use `RUN_ITERATIONS = 1` to
// avoid result-averaging for better statistical analysis
pub struct Sec1DecodePointCompressedRun;

impl CostRunner for Sec1DecodePointCompressedRun {
    const COST_TYPE: CostType =
        CostType::Experimental(ExperimentalCostType::Sec1DecodePointCompressed);

    // we want to capture the output variance w.r.t the random input, thus we
    // set `RUN_ITERATIONS` to 1.
    const RUN_ITERATIONS: u64 = 1;

    type SampleType = Sec1DecodePointSample;

    type RecycledType = (Self::SampleType, Option<VerifyingKey>);

    fn run_iter(host: &crate::Host, iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        Sec1DecodePointUncompressedRun::run_iter(host, iter, sample)
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
        Sec1DecodePointUncompressedRun::run_baseline_iter(host, iter, sample)
    }
}
