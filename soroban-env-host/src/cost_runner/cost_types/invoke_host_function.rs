use crate::{budget::CostType, cost_runner::CostRunner, Vm};
use std::rc::Rc;

pub struct InvokeHostFunctionRun;

impl CostRunner for InvokeHostFunctionRun {
    const COST_TYPE: CostType = CostType::InvokeHostFunction;
    type SampleType = Rc<Vm>;

    const RUN_ITERATIONS: u64 = 100;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.invoke_function_raw(host, "test", &[]).unwrap();
    }
}
