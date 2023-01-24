use crate::{budget::CostType, cost_runner::CostRunner};

pub struct ComputeEd25519PubKeyRun;

impl CostRunner for ComputeEd25519PubKeyRun {
    const COST_TYPE: CostType = CostType::ComputeEd25519PubKey;
    type SampleType = Vec<u8>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        host.ed25519_pub_key_from_bytes(sample.as_slice())
            .expect("publickey");
    }
}
