use crate::{
    budget::{AsBudget, CostType},
    cost_runner::CostRunner,
    MeteredVector,
};

pub struct HostMemAllocRun;

impl CostRunner for HostMemAllocRun {
    const COST_TYPE: CostType = CostType::HostMemAlloc;
    type SampleType = u64;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        // we just create a single MeteredVector with capacity to see what the
        // mem allocation cost is.
        MeteredVector::<u64>::with_capacity(sample as usize, host.as_budget()).unwrap();
    }
}
