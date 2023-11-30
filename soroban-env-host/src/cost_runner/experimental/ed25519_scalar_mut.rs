use std::hint::black_box;

use crate::{
    budget::CostTracker,
    cost_runner::{CostRunner, CostType},
};
use curve25519_dalek::{edwards, scalar};

use super::ExperimentalCostType;

pub struct Ed25519ScalarMulRun;

#[allow(non_snake_case)]
#[derive(Clone)]
pub struct Ed25519ScalarMulSample {
    pub a: scalar::Scalar,
    pub A: edwards::EdwardsPoint,
    pub b: scalar::Scalar,
}

impl CostRunner for Ed25519ScalarMulRun {
    const COST_TYPE: CostType =
        CostType::Experimental(ExperimentalCostType::EdwardsPointCurve25519ScalarMul);

    const RUN_ITERATIONS: u64 = 100;

    type SampleType = Ed25519ScalarMulSample;

    type RecycledType = Self::SampleType;

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        black_box(edwards::EdwardsPoint::vartime_double_scalar_mul_basepoint(
            &sample.a, &sample.A, &sample.b,
        ));
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
