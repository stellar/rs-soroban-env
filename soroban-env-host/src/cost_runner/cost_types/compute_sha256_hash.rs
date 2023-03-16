use crate::{budget::CostType, cost_runner::CostRunner};

pub struct ComputeSha256HashRun;

impl CostRunner for ComputeSha256HashRun {
    const COST_TYPE: CostType = CostType::ComputeSha256Hash;
    type SampleType = Vec<u8>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        host.sha256_hash_from_bytes(sample.as_slice())
            .expect("sha256");
        // `forget` avoids deallocation of sample which artificially inflates the cost
        std::mem::forget(sample)
    }
}
