use crate::{
    budget::{AsBudget, CostType},
    cost_runner::CostRunner,
    events::InternalEvent,
    host::metered_clone::MeteredClone,
};

pub struct EventCloneRun;

impl CostRunner for EventCloneRun {
    const COST_TYPE: CostType = CostType::EventClone;

    const RUN_ITERATIONS: u64 = 100;

    type SampleType = InternalEvent;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.metered_clone(host.as_budget()).expect("event clone");
    }
}
