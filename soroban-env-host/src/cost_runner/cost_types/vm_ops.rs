use crate::{
    cost_runner::{CostRunner, CostType},
    xdr::ContractCostType::VmInstantiation,
    xdr::Hash,
    Vm,
};
use std::{hint::black_box, rc::Rc};

pub struct VmInstantiationRun;

#[derive(Clone)]
pub struct VmInstantiationSample {
    pub id: Option<Hash>,
    pub wasm: Vec<u8>,
}

impl CostRunner for VmInstantiationRun {
    const COST_TYPE: CostType = CostType::Contract(VmInstantiation);

    const RUN_ITERATIONS: u64 = 10;

    type SampleType = VmInstantiationSample;

    type RecycledType = (Option<Rc<Vm>>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let vm = black_box(Vm::new(host, sample.id.unwrap(), &sample.wasm[..]).unwrap());
        (Some(vm), sample.wasm)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(VmInstantiation, Some(0)).unwrap());
        black_box((None, sample.wasm))
    }
}
