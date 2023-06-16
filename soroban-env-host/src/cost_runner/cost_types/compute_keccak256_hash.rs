use std::hint::black_box;

use crate::{cost_runner::CostRunner, xdr::ContractCostType};

pub struct ComputeKeccak256HashRun;

impl CostRunner for ComputeKeccak256HashRun {
    const COST_TYPE: ContractCostType = ContractCostType::ComputeKeccak256Hash;

    type SampleType = Vec<u8>;

    type RecycledType = (Option<Vec<u8>>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let hash = black_box(
            host.keccak256_hash_from_bytes(sample.as_slice())
                .expect("keccak256"),
        );
        (Some(hash), sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, Some(0)).unwrap());
        black_box((None, sample))
    }
}
