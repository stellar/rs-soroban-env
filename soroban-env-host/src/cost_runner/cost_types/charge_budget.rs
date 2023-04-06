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
        black_box(
            host.as_budget()
                .charge(CostType::WasmInsnExec, sample, None)
                .unwrap(),
        );
    }

    fn run_baseline_iter(host: &crate::Host, _iter: u64, sample: u64) {
        black_box(
            host.as_budget()
                .get_tracker_mut(Self::COST_TYPE, |(t_iters, _)| {
                    Ok(*t_iters = t_iters.saturating_add(1))
                })
                .unwrap(),
        );
        black_box(sample);
    }
}
