use crate::{cost_runner::CostRunner, xdr::ContractCostType, Symbol, Val, Vm};
use std::{hint::black_box, rc::Rc};

pub struct InvokeVmFunctionRun;

const TEST_SYM: Symbol = match Symbol::try_from_small_str("test") {
    Ok(s) => s,
    _ => panic!(),
};

impl CostRunner for InvokeVmFunctionRun {
    const COST_TYPE: ContractCostType = ContractCostType::InvokeVmFunction;

    type SampleType = Rc<Vm>;

    type RecycledType = (Option<Val>, Rc<Vm>);

    const RUN_ITERATIONS: u64 = 100;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let rv = black_box(sample.invoke_function_raw(host, &TEST_SYM, &[]).unwrap());
        (Some(rv), sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, None).unwrap());
        black_box((None, sample))
    }
}

pub struct InvokeHostFunctionRun;

impl CostRunner for InvokeHostFunctionRun {
    const COST_TYPE: ContractCostType = ContractCostType::InvokeHostFunction;

    const RUN_ITERATIONS: u64 = 1;

    type SampleType = Rc<Vm>;

    type RecycledType = (Option<Val>, Rc<Vm>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let rv = black_box(sample.invoke_function_raw(host, &TEST_SYM, &[]).unwrap());
        (Some(rv), sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, None).unwrap());
        black_box((None, sample))
    }
}
