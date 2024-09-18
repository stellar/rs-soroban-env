use ark_bls12_381::{Fq, Fq2, G1Affine, G2Affine};

use super::ExperimentalCostType::*;
use crate::{
    budget::CostTracker,
    cost_runner::{CostRunner, CostType},
    xdr::ContractCostType,
    Host, HostError,
};
use std::hint::black_box;

pub struct Bls12381G1AffineSerializeUncompressedRun;
pub struct Bls12381G2AffineSerializeUncompressedRun;
pub struct Bls12381G1AffineDeserializeUncompressedRun;
pub struct Bls12381G2AffineDeserializeUncompressedRun;
pub struct Bls12381Fp2DeserializeUncompressedRun;

// ser/deser

macro_rules! impl_ser_runner_for_bls {
    ($runner: ident, $cost: ident, $expected_size: literal, $sample: ident) => {
        impl CostRunner for $runner {
            const COST_TYPE: CostType = CostType::Experimental($cost);

            const RUN_ITERATIONS: u64 = 1;

            type SampleType = $sample;

            type RecycledType = (Option<$sample>, Option<Vec<u8>>);

            fn run_iter(host: &Host, _iter: u64, sample: $sample) -> Self::RecycledType {
                let mut buf = vec![0u8; 1000];
                let _ = host
                    .serialize_uncompressed_into_slice::<$expected_size, _>(
                        &sample, &mut buf, "test",
                    )
                    .unwrap();
                black_box((None, Some(buf)))
            }

            fn run_baseline_iter(host: &Host, _iter: u64, sample: $sample) -> Self::RecycledType {
                black_box(
                    host.charge_budget(ContractCostType::Int256AddSub, None)
                        .unwrap(),
                );
                black_box((Some(sample), None))
            }

            fn get_tracker(_host: &Host, _sample: &$sample) -> CostTracker {
                CostTracker {
                    iterations: Self::RUN_ITERATIONS,
                    inputs: None,
                    cpu: 0,
                    mem: 0,
                }
            }
        }
    };
}

impl_ser_runner_for_bls!(
    Bls12381G1AffineSerializeUncompressedRun,
    Bls12381G1AffineSerializeUncompressed,
    96,
    G1Affine
);
impl_ser_runner_for_bls!(
    Bls12381G2AffineSerializeUncompressedRun,
    Bls12381G2AffineSerializeUncompressed,
    192,
    G2Affine
);

macro_rules! impl_deser_runner_for_bls {
    ($runner: ident, $cost: ident, $expected_size: literal, $rt: ty) => {
        impl CostRunner for $runner {
            const COST_TYPE: CostType = CostType::Experimental($cost);

            const RUN_ITERATIONS: u64 = 1;

            type SampleType = Vec<u8>;

            type RecycledType = (Option<Self::SampleType>, Option<$rt>);

            fn run_iter(host: &Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
                let res = host
                    .deserialize_uncompressed_no_validate::<$expected_size, _>(&sample, "test")
                    .unwrap();
                black_box((None, Some(res)))
            }

            fn run_baseline_iter(
                host: &Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                black_box(
                    host.charge_budget(ContractCostType::Int256AddSub, None)
                        .unwrap(),
                );
                black_box((Some(sample), None))
            }

            fn get_tracker(_host: &Host, _sample: &Self::SampleType) -> CostTracker {
                CostTracker {
                    iterations: Self::RUN_ITERATIONS,
                    inputs: None,
                    cpu: 0,
                    mem: 0,
                }
            }
        }
    };
}

impl_deser_runner_for_bls!(
    Bls12381G1AffineDeserializeUncompressedRun,
    Bls12381G1AffineDeserializeUncompressed,
    2,
    G1Affine
);
impl_deser_runner_for_bls!(
    Bls12381G2AffineDeserializeUncompressedRun,
    Bls12381G2AffineDeserializeUncompressed,
    4,
    G2Affine
);
impl_deser_runner_for_bls!(
    Bls12381Fp2DeserializeUncompressedRun,
    Bls12381Fp2DeserializeUncompressed,
    2,
    Fq2
);

#[macro_export]
macro_rules! impl_experiment_const_cost_runner_for_bls_deref_sample {
    ($runner: ident, $cost: ident, $host_fn: ident, $sample: ident, $rt: ty, $($arg: ident),*) => {
        impl CostRunner for $runner {
            const COST_TYPE: CostType = CostType::Experimental($cost);

            const RUN_ITERATIONS: u64 = 1;

            type SampleType = $sample;

            type RecycledType = (Option<$sample>, Option<$rt>);

            fn run_iter(host: &Host, _iter: u64, mut sample: $sample) -> Self::RecycledType {
                let $sample($( $arg ),*) = &mut sample;
                let res = host.$host_fn($($arg),*).unwrap();
                black_box((Some(sample), Some(res)))
            }

            fn run_baseline_iter(
                host: &Host,
                _iter: u64,
                sample: $sample,
            ) -> Self::RecycledType {
                black_box(
                    host.charge_budget(ContractCostType::Int256AddSub, None)
                        .unwrap(),
                );
                black_box((Some(sample), None))
            }

            fn get_tracker(_host: &Host, _sample: &$sample) -> CostTracker {
                CostTracker {
                    iterations: Self::RUN_ITERATIONS,
                    inputs: None,
                    cpu: 0,
                    mem: 0,
                }
            }
        }
    };
}

impl Host {
    fn g1_compute_y_from_x(&self, pt: &G1Affine) -> Result<Fq, HostError> {
        Ok(G1Affine::get_ys_from_x_unchecked(pt.x).unwrap().0)
    }
    fn g2_compute_y_from_x(&self, pt: &G2Affine) -> Result<Fq2, HostError> {
        Ok(G2Affine::get_ys_from_x_unchecked(pt.x).unwrap().0)
    }
}

pub struct Bls12381G1ComputeYFromXRun;

#[derive(Clone)]
pub struct Bls12381G1ComputeYFromXSample(pub G1Affine);

impl_experiment_const_cost_runner_for_bls_deref_sample!(
    Bls12381G1ComputeYFromXRun,
    Bls12381G1ComputeYFromX,
    g1_compute_y_from_x,
    Bls12381G1ComputeYFromXSample,
    Fq,
    pt
);

pub struct Bls12381G2ComputeYFromXRun;

#[derive(Clone)]
pub struct Bls12381G2ComputeYFromXSample(pub G2Affine);

impl_experiment_const_cost_runner_for_bls_deref_sample!(
    Bls12381G2ComputeYFromXRun,
    Bls12381G2ComputeYFromX,
    g2_compute_y_from_x,
    Bls12381G2ComputeYFromXSample,
    Fq2,
    pt
);
