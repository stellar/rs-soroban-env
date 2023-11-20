use std::hint::black_box;

use crate::{
    cost_runner::{CostRunner, CostType},
    host::crypto::sha256_hash_from_bytes_raw,
    xdr::ContractCostType::ComputeSha256Hash,
};

pub struct ComputeSha256HashRun;

impl CostRunner for ComputeSha256HashRun {
    const COST_TYPE: CostType = CostType::Contract(ComputeSha256Hash);

    type SampleType = Vec<u8>;

    type RecycledType = (Option<[u8; 32]>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let hash = black_box(sha256_hash_from_bytes_raw(sample.as_slice(), host).expect("sha256"));
        (Some(hash), sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(ComputeSha256Hash, Some(0)).unwrap());
        black_box((None, sample))
    }
}
