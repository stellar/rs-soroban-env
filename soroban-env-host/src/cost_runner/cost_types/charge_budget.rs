use std::hint::black_box;

use crate::{
    budget::{AsBudget, CostType},
    cost_runner::CostRunner,
};

pub struct ChargeBudgetRun;

impl CostRunner for ChargeBudgetRun {
    const COST_TYPE: CostType = CostType::ChargeBudget;

    const RUN_ITERATIONS: u64 = 1000;

    type SampleType = u64;

    type RecycledType = ();

    fn run_iter(host: &crate::Host, _iter: u64, sample: u64) {
        // The `CostType` here is irrelevant, can pass in any type except `CostType::ChargeBudget`.
        black_box(
            host.as_budget()
                .charge(CostType::WasmInsnExec, sample)
                .unwrap(),
        );
    }

    fn run_baseline_iter(_host: &crate::Host, _iter: u64, sample: u64) {
        black_box(sample);
    }
}
