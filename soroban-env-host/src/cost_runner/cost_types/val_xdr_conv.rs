use std::hint::black_box;

use crate::{cost_runner::CostRunner, xdr::ContractCostType, xdr::ScVal, Val};

pub struct ValXdrConvRun;

impl CostRunner for ValXdrConvRun {
    const COST_TYPE: ContractCostType = ContractCostType::ValXdrConv;

    type SampleType = (Option<Val>, ScVal);

    type RecycledType = (Option<(ScVal, Val)>, ScVal);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let sv = black_box(host.from_host_val(sample.0.unwrap()).unwrap());
        let rv = black_box(host.to_host_val(&sample.1).unwrap());
        (Some((sv, rv)), sample.1)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, None).unwrap());
        black_box((None, sample.1))
    }
}
