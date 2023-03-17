use ed25519_dalek::PublicKey;

use crate::{budget::CostType, cost_runner::CostRunner};

pub struct ComputeEd25519PubKeyRun;

impl CostRunner for ComputeEd25519PubKeyRun {
    const COST_TYPE: CostType = CostType::ComputeEd25519PubKey;

    type SampleType = Vec<u8>;

    type RecycledType = (Option<PublicKey>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let pk = host
            .ed25519_pub_key_from_bytes(sample.as_slice())
            .expect("publickey");
        (Some(pk), sample)
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        (None, sample)
    }
}
