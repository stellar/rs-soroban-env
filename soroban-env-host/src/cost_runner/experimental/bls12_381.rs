use ark_bls12_381::{Fq2, G1Affine, G2Affine};

use super::ExperimentalCostType::*;
use crate::{
    budget::CostTracker,
    cost_runner::{CostRunner, CostType},
};
use std::hint::black_box;

pub struct Bls12381G1AffineSerializeUncompressedRun;
pub struct Bls12381G2AffineSerializeUncompressedRun;
pub struct Bls12381G1AffineDeserializeUncompressedRun;
pub struct Bls12381G2AffineDeserializeUncompressedRun;
pub struct Bls12381Fp2DeserializeUncompressedRun;

// ser/deser

macro_rules! impl_ser_runner_for_bls {
    ($runner: ident, $cost: ident, $units: literal, $sample: ident) => {
        impl CostRunner for $runner {
            const COST_TYPE: CostType = CostType::Experimental($cost);

            const RUN_ITERATIONS: u64 = 1;

            type SampleType = $sample;

            type RecycledType = (Option<$sample>, Option<Vec<u8>>);

            fn run_iter(host: &crate::Host, _iter: u64, sample: $sample) -> Self::RecycledType {
                let mut buf = vec![0u8; 1000];
                let _ = host
                    .serialize_uncompressed_into_slice(&sample, &mut buf, $units, "test")
                    .unwrap();
                black_box((None, Some(buf)))
            }

            fn run_baseline_iter(
                host: &crate::Host,
                _iter: u64,
                sample: $sample,
            ) -> Self::RecycledType {
                black_box(
                    host.charge_budget(crate::xdr::ContractCostType::Int256AddSub, None)
                        .unwrap(),
                );
                black_box((Some(sample), None))
            }

            fn get_tracker(_host: &crate::Host, _sample: &$sample) -> CostTracker {
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
    2,
    G1Affine
);
impl_ser_runner_for_bls!(
    Bls12381G2AffineSerializeUncompressedRun,
    Bls12381G2AffineSerializeUncompressed,
    4,
    G2Affine
);

macro_rules! impl_deser_runner_for_bls {
    ($runner: ident, $cost: ident, $units: literal, $rt: ty) => {
        impl CostRunner for $runner {
            const COST_TYPE: CostType = CostType::Experimental($cost);

            const RUN_ITERATIONS: u64 = 1;

            type SampleType = Vec<u8>;

            type RecycledType = (Option<Self::SampleType>, Option<$rt>);

            fn run_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                let res = host
                    .deserialize_uncompessed_no_validate(&sample, $units, "test")
                    .unwrap();
                black_box((None, Some(res)))
            }

            fn run_baseline_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                black_box(
                    host.charge_budget(crate::xdr::ContractCostType::Int256AddSub, None)
                        .unwrap(),
                );
                black_box((Some(sample), None))
            }

            fn get_tracker(_host: &crate::Host, _sample: &Self::SampleType) -> CostTracker {
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
