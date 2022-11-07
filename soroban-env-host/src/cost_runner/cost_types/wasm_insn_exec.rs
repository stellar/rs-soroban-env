use crate::{
    budget::CostType,
    cost_runner::CostRunner,
    xdr::{ScVal, ScVec},
    Vm,
};
use std::rc::Rc;

pub struct WasmInsnExecRun;

pub struct WasmInsnExecSample {
    pub insns: u64,
    pub args: ScVec,
    pub vm: Rc<Vm>,
}

impl CostRunner for WasmInsnExecRun {
    const COST_TYPE: CostType = CostType::WasmInsnExec;
    type SampleType = WasmInsnExecSample;

    fn run_iter(host: &crate::Host, _iter: u64, sample: &mut Self::SampleType) {
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
