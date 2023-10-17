use std::hint::black_box;

use rand_chacha::ChaCha20Rng;

use crate::{
    cost_runner::{CostRunner, CostType},
    host::crypto::chacha20_fill_bytes,
    xdr::ContractCostType::ChaCha20DrawBytes,
};

pub struct ChaCha20DrawBytesRun;

impl CostRunner for ChaCha20DrawBytesRun {
    const COST_TYPE: CostType = CostType::Contract(ChaCha20DrawBytes);

    type SampleType = (ChaCha20Rng, Vec<u8>);

    type RecycledType = Self::SampleType;

    fn run_iter(
        host: &crate::Host,
        _iter: u64,
        mut sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(
            chacha20_fill_bytes(&mut sample.0, sample.1.as_mut_slice(), host)
                .expect("chacha20 draw byte"),
        );
        black_box(sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(ChaCha20DrawBytes, Some(0)).unwrap());
        black_box(sample)
    }
}
