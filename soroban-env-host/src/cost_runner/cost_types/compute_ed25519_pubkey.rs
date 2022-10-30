use crate::{budget::CostType, cost_runner::CostRunner};

pub struct ComputeEd25519PubKeyRun;

impl CostRunner for ComputeEd25519PubKeyRun {
    const COST_TYPE: CostType = CostType::ComputeEd25519PubKey;
    type SampleType = Vec<u8>;

    fn run_iter(_host: &crate::Host, _iter: u64, sample: &mut Self::SampleType) -> Option<u64> {
        ed25519_dalek::PublicKey::from_bytes(sample.as_slice()).expect("publickey");
        // input is ignored
        Some(1)
    }
}
