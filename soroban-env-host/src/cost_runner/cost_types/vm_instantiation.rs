use crate::{budget::CostType, cost_runner::CostRunner, xdr::Hash, Vm};

pub struct VmInstantiationRun;

pub struct VmInstantiationSample {
    pub id: Hash,
    pub wasm: Vec<u8>,
}

impl CostRunner for VmInstantiationRun {
    const COST_TYPE: CostType = CostType::VmInstantiation;
    const RUN_ITERATIONS: u64 = 10;
    type SampleType = VmInstantiationSample;

    fn run_iter(host: &crate::Host, _iter: u64, sample: &mut Self::SampleType) {
        Vm::new(host, sample.id.clone(), &sample.wasm[..]).unwrap();
    }
}
