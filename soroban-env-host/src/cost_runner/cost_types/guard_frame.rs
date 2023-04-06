use std::hint::black_box;

use soroban_env_common::RawVal;

use crate::{budget::CostType, cost_runner::CostRunner, host::Frame, xdr::Hash, Status, Symbol};

pub struct GuardFrameRun;

impl CostRunner for GuardFrameRun {
    const COST_TYPE: CostType = CostType::GuardFrame;

    type SampleType = (Hash, Symbol);

    type RecycledType = Option<RawVal>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let rv = black_box(
            host.with_frame(Frame::Token(sample.0, sample.1, vec![]), || {
                Ok(Status::OK.to_raw())
            })
            .unwrap(),
        );
        Some(rv)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        _sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, 1, None).unwrap());
        black_box(None)
    }
}
