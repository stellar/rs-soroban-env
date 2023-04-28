use std::{cmp::Ordering, hint::black_box};

use soroban_env_common::Compare;

use crate::{budget::AsBudget, cost_runner::CostRunner, xdr::ContractCostType};

pub struct HostMemCmpRun;
impl CostRunner for HostMemCmpRun {
    const COST_TYPE: ContractCostType = ContractCostType::HostMemCmp;

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
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, Some(0)).unwrap());
        black_box((None, sample))
    }
}
