use std::hint::black_box;

use ed25519_dalek::PublicKey;

use crate::{budget::CostType, cost_runner::CostRunner};

pub struct ComputeEd25519PubKeyRun;

impl CostRunner for ComputeEd25519PubKeyRun {
    const COST_TYPE: CostType = CostType::ComputeEd25519PubKey;

    type SampleType = Vec<u8>;

    type RecycledType = (Option<PublicKey>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let pk = black_box(
            host.ed25519_pub_key_from_bytes(sample.as_slice())
                .expect("publickey"),
        );
        (Some(pk), sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, 1, None).unwrap());
        black_box((None, sample))
    }
}
