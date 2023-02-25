use crate::{
    budget::{AsBudget, CostType},
    cost_runner::CostRunner,
    host::metered_clone::MeteredClone,
};

pub struct HostMemCpyRun;

impl CostRunner for HostMemCpyRun {
    const COST_TYPE: CostType = CostType::HostMemCpy;
    type SampleType = ([u8; 32], u64);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        for _ in 0..sample.1 {
            sample.0.metered_clone(host.as_budget()).unwrap();
        }
    }
}
