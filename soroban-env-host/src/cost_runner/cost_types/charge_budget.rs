use crate::{
    budget::{AsBudget, CostType},
    cost_runner::CostRunner,
};

pub struct ChargeBudgetRun;

impl CostRunner for ChargeBudgetRun {
    const COST_TYPE: CostType = CostType::ChargeBudget;

    const RUN_ITERATIONS: u64 = 1000;

    type SampleType = u64;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        // The `CostType` here is irrelevant, can pass in any type except `CostType::ChargeBudget`.
        host.as_budget()
            .charge(CostType::WasmInsnExec, sample)
            .unwrap();
    }
}
