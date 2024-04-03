use crate::{
    cost_runner::{CostRunner, CostType},
    xdr::ContractCostType::Sec1DecodePointUncompressed,
};
use p256::ecdsa::VerifyingKey;
use std::hint::black_box;

pub struct Sec1DecodePointUncompressedRun;

#[allow(non_snake_case)]
#[derive(Clone)]
pub struct Sec1DecodePointSample {
    pub bytes: Box<[u8]>,
}

impl CostRunner for Sec1DecodePointUncompressedRun {
    const COST_TYPE: CostType = CostType::Contract(Sec1DecodePointUncompressed);

    type SampleType = Sec1DecodePointSample;

    type RecycledType = (Self::SampleType, Option<VerifyingKey>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let vk = black_box(
            host.secp256r1_decode_sec1_uncompressed_pubkey(&sample.bytes)
                .unwrap(),
        );
        black_box((sample, Some(vk)))
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(
            host.charge_budget(Sec1DecodePointUncompressed, None)
                .unwrap(),
        );
        black_box((sample, None))
    }
}
