use std::{cmp::Ordering, hint::black_box};

use soroban_env_common::Compare;

use crate::{
    budget::{AsBudget, CostType},
    cost_runner::CostRunner,
};

pub struct HostMemCmpRun;
impl CostRunner for HostMemCmpRun {
    const COST_TYPE: CostType = CostType::HostMemCmp;

    type SampleType = (Vec<u8>, Vec<u8>);

    type RecycledType = (Option<Ordering>, Self::SampleType);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let ord = black_box(
            host.as_budget()
                .compare(&sample.0.as_slice(), &sample.1.as_slice())
                .unwrap(),
        );
        (Some(ord), sample)
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box((None, sample))
    }
}
