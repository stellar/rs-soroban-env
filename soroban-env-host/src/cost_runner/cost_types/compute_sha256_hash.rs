use std::hint::black_box;

use crate::{cost_runner::CostRunner, xdr::ContractCostType};

pub struct ComputeSha256HashRun;

impl CostRunner for ComputeSha256HashRun {
    const COST_TYPE: ContractCostType = ContractCostType::ComputeSha256Hash;

    type SampleType = Vec<u8>;

    type RecycledType = (Option<Vec<u8>>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let hash = black_box(
            host.sha256_hash_from_bytes(sample.as_slice())
                .expect("sha256"),
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
