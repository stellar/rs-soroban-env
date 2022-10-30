use crate::{budget::CostType, cost_runner::CostRunner};
use sha2::{Digest, Sha256};

pub struct ComputeSha256HashRun;

impl CostRunner for ComputeSha256HashRun {
    const COST_TYPE: CostType = CostType::ComputeSha256Hash;
    type SampleType = Vec<u8>;

    fn run_iter(_host: &crate::Host, _iter: u64, sample: &mut Self::SampleType) -> Option<u64> {
        let input = sample.len() as u64;
        Sha256::digest(sample).as_slice().to_vec();
        Some(input)
    }
}
