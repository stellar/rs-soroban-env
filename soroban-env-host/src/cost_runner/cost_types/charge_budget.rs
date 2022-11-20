use crate::{budget::Budget, budget::CostType, cost_runner::CostRunner};

pub struct ChargeBudgetRun;

impl CostRunner for ChargeBudgetRun {
    const COST_TYPE: CostType = CostType::ChargeBudget;

    const RUN_ITERATIONS: u64 = 1000;

    type SampleType = (Budget, u64);

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        // The `CostType` here is irrelevant, can pass in any type except `CostType::ChargeBudget`.
        sample.0.charge(CostType::WasmInsnExec, sample.1).unwrap();
    }

    fn get_total_input(_host: &crate::Host, _sample: &Self::SampleType) -> u64 {
        Self::RUN_ITERATIONS
    }
}
