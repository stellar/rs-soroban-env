use crate::{
    budget::CostType,
    cost_runner::CostRunner,
    xdr::{ScVal, ScVec},
    Vm,
};
use std::rc::Rc;

pub struct WasmInsnExecRun;

#[derive(Clone)]
pub struct WasmInsnExecSample {
    pub insns: u64,
    pub args: ScVec,
    pub vm: Rc<Vm>,
}

impl CostRunner for WasmInsnExecRun {
    const COST_TYPE: CostType = CostType::WasmInsnExec;
    type SampleType = WasmInsnExecSample;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        let scval = sample
            .vm
            .invoke_function(host, "test", &sample.args)
            .unwrap();
        assert_eq!(scval, ScVal::Symbol("pass".try_into().unwrap()));
    }

    fn get_total_input(_host: &crate::Host, sample: &Self::SampleType) -> u64 {
        sample.insns * 4 // TODO: avoid magic number
    }
}

pub struct WasmMemAllocRun;
impl CostRunner for WasmMemAllocRun {
    const COST_TYPE: CostType = CostType::WasmMemAlloc;

    const RUN_ITERATIONS: u64 = 1;

    type SampleType = (Rc<Vm>, usize);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.0.invoke_function_raw(host, "test", &[]).unwrap();
    }

    fn get_total_input(_host: &crate::Host, sample: &Self::SampleType) -> u64 {
        (sample.1 as u64) * Self::RUN_ITERATIONS * 65536
    }
}
