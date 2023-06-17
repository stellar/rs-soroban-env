use std::hint::black_box;

use crate::{cost_runner::CostRunner, host::Frame, xdr::ContractCostType, xdr::Hash, Symbol, Val};

pub struct GuardFrameRun;

impl CostRunner for GuardFrameRun {
    const COST_TYPE: ContractCostType = ContractCostType::GuardFrame;

    type SampleType = (Hash, Symbol);

    type RecycledType = Option<Self::SampleType>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        black_box(
            host.with_frame(Frame::Token(sample.0, sample.1, vec![]), || {
                Ok(Val::VOID.to_val())
            })
            .unwrap(),
        );
        black_box(None)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, None).unwrap());
        black_box(Some(sample))
    }
}
