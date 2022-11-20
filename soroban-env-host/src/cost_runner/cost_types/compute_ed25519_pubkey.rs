use crate::{budget::CostType, cost_runner::CostRunner};

pub struct ComputeEd25519PubKeyRun;

impl CostRunner for ComputeEd25519PubKeyRun {
    const COST_TYPE: CostType = CostType::ComputeEd25519PubKey;
    type SampleType = Vec<u8>;

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        ed25519_dalek::PublicKey::from_bytes(sample.as_slice()).expect("publickey");
    }

    fn get_total_input(_host: &crate::Host, _sample: &Self::SampleType) -> u64 {
        // input is ignored
        Self::RUN_ITERATIONS
    }
}
