use std::hint::black_box;

use crate::{budget::CostType, cost_runner::CostRunner, xdr::ScVal, RawVal};

pub struct ValXdrConvRun;

impl CostRunner for ValXdrConvRun {
    const COST_TYPE: CostType = CostType::ValXdrConv;

    type SampleType = (Option<RawVal>, ScVal);

    type RecycledType = (Option<(ScVal, RawVal)>, ScVal);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let sv = black_box(host.from_host_val(sample.0.unwrap()).unwrap());
        let rv = black_box(host.to_host_val(&sample.1).unwrap());
        (Some((sv, rv)), sample.1)
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box((None, sample.1))
    }
}
