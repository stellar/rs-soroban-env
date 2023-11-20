use std::hint::black_box;

use crate::{
    cost_runner::{CostRunner, CostType},
    xdr::ContractCostType::ComputeKeccak256Hash,
};

pub struct ComputeKeccak256HashRun;

impl CostRunner for ComputeKeccak256HashRun {
    const COST_TYPE: CostType = CostType::Contract(ComputeKeccak256Hash);

    type SampleType = Vec<u8>;

    type RecycledType = (Option<[u8; 32]>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let hash = black_box(
            host.keccak256_hash_from_bytes_raw(sample.as_slice())
                .expect("keccak256"),
        );
        (Some(hash), sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(ComputeKeccak256Hash, Some(0)).unwrap());
        black_box((None, sample))
    }
}
