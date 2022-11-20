use crate::{budget::CostType, cost_runner::CostRunner, Vm};
use std::rc::Rc;

pub struct InvokeVmFunctionRun;

impl CostRunner for InvokeVmFunctionRun {
    const COST_TYPE: CostType = CostType::InvokeVmFunction;
    type SampleType = Rc<Vm>;

    const RUN_ITERATIONS: u64 = 100;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.invoke_function_raw(host, "test", &[]).unwrap();
    }
}

pub struct InvokeHostFunctionRun;

impl CostRunner for InvokeHostFunctionRun {
    const COST_TYPE: CostType = CostType::InvokeHostFunction;

    const RUN_ITERATIONS: u64 = 1;

    type SampleType = Rc<Vm>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.invoke_function_raw(host, "test", &[]).unwrap();
    }
}
