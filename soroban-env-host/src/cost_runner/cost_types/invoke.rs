use crate::{budget::CostType, cost_runner::CostRunner, RawVal, Symbol, Vm};
use std::rc::Rc;

pub struct InvokeVmFunctionRun;

const TEST_SYM: Symbol = match Symbol::try_from_small_str("test") {
    Ok(s) => s,
    _ => panic!(),
};

impl CostRunner for InvokeVmFunctionRun {
    const COST_TYPE: CostType = CostType::InvokeVmFunction;

    type SampleType = Rc<Vm>;

    type RecycledType = (Option<RawVal>, Rc<Vm>);

    const RUN_ITERATIONS: u64 = 100;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let rv = sample.invoke_function_raw(host, &TEST_SYM, &[]).unwrap();
        (Some(rv), sample)
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        (None, sample)
    }
}

pub struct InvokeHostFunctionRun;

impl CostRunner for InvokeHostFunctionRun {
    const COST_TYPE: CostType = CostType::InvokeHostFunction;

    const RUN_ITERATIONS: u64 = 1;

    type SampleType = Rc<Vm>;

    type RecycledType = (Option<RawVal>, Rc<Vm>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let rv = sample.invoke_function_raw(host, &TEST_SYM, &[]).unwrap();
        (Some(rv), sample)
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        (None, sample)
    }
}
