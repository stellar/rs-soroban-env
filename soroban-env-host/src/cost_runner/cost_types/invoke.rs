use soroban_env_common::ConversionError;
use wasmi::Value;

use crate::{
    cost_runner::{CostRunner, CostType},
    vm::dummy0,
    xdr::ContractCostType::{DispatchHostFunction, InvokeVmFunction},
    HostError, Symbol, Val, Vm,
};
use std::{hint::black_box, rc::Rc};

pub struct InvokeVmFunctionRun;

const TEST_SYM: Symbol = match Symbol::try_from_small_str("test") {
    Ok(s) => s,
    _ => panic!(),
};

impl CostRunner for InvokeVmFunctionRun {
    const COST_TYPE: CostType = CostType::Contract(InvokeVmFunction);

    type SampleType = (Rc<Vm>, Vec<Value>);

    type RecycledType = (Option<Val>, Self::SampleType);

    const RUN_ITERATIONS: u64 = 100;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let rv = black_box(
            sample
                .0
                .metered_func_call(host, &TEST_SYM, sample.1.as_slice())
                .unwrap(),
        );
        (Some(rv), sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(InvokeVmFunction, None).unwrap());
        black_box((None, sample))
    }
}

pub struct InvokeHostFunctionRun;

impl CostRunner for InvokeHostFunctionRun {
    const COST_TYPE: CostType = CostType::Contract(DispatchHostFunction);

    const RUN_ITERATIONS: u64 = 1000;

    type SampleType = Rc<Vm>;

    type RecycledType = Rc<Vm>;

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        black_box(
            sample
                .with_caller(|caller| dummy0(caller).map_err(|_| HostError::from(ConversionError)))
                .unwrap(),
        );
        black_box(sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(DispatchHostFunction, None).unwrap());
        black_box(sample)
    }
}
