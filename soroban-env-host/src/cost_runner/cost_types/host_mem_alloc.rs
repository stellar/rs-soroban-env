use std::hint::black_box;

use crate::{budget::AsBudget, cost_runner::CostRunner, xdr::ContractCostType, MeteredVector};

pub struct MemAllocRun;

impl CostRunner for MemAllocRun {
    const COST_TYPE: ContractCostType = ContractCostType::MemAlloc;

    type SampleType = u64;

    type RecycledType = Option<MeteredVector<u64>>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        // we just create a single MeteredVector with capacity to see what the
        // mem allocation cost is.
        black_box(Some(
            MeteredVector::<u64>::with_capacity(sample as usize, host.as_budget()).unwrap(),
        ))
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        _sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, Some(0)).unwrap());
        black_box(None)
    }
}
