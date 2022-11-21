use crate::{budget::CostType, cost_runner::CostRunner};
use curve25519_dalek::{edwards, scalar};

pub struct Ed25519ScalarMulRun;

#[allow(non_snake_case)]
#[derive(Clone)]
pub struct Ed25519ScalarMulSample {
    pub a: scalar::Scalar,
    pub A: edwards::EdwardsPoint,
    pub b: scalar::Scalar,
}

impl CostRunner for Ed25519ScalarMulRun {
    const COST_TYPE: CostType = CostType::EdwardsPointCurve25519ScalarMul;
    type SampleType = Ed25519ScalarMulSample;

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        edwards::EdwardsPoint::vartime_double_scalar_mul_basepoint(&sample.a, &sample.A, &sample.b);
    }

    fn get_total_input(_host: &crate::Host, _sample: &Self::SampleType) -> u64 {
        // input is ignored
        Self::RUN_ITERATIONS
    }
}
