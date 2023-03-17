use crate::{budget::CostType, cost_runner::CostRunner};

pub struct ComputeSha256HashRun;

impl CostRunner for ComputeSha256HashRun {
    const COST_TYPE: CostType = CostType::ComputeSha256Hash;

    type SampleType = Vec<u8>;

    type RecycledType = (Option<Vec<u8>>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let hash = host
            .sha256_hash_from_bytes(sample.as_slice())
            .expect("sha256");
        (Some(hash), sample)
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        (None, sample)
    }
}
