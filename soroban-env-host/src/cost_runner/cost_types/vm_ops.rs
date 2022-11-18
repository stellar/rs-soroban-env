use crate::{budget::CostType, cost_runner::CostRunner, xdr::Hash, Vm};
use std::rc::Rc;

pub struct VmInstantiationRun;

#[derive(Clone)]
pub struct VmInstantiationSample {
    pub id: Hash,
    pub wasm: Vec<u8>,
}

impl CostRunner for VmInstantiationRun {
    const COST_TYPE: CostType = CostType::VmInstantiation;
    const RUN_ITERATIONS: u64 = 10;
    type SampleType = VmInstantiationSample;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        Vm::new(host, sample.id, &sample.wasm[..]).unwrap();
    }
}

pub struct VmMemReadRun;
impl CostRunner for VmMemReadRun {
    const COST_TYPE: CostType = CostType::VmMemRead;

    type SampleType = (Rc<Vm>, usize);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        let mut buf: Vec<u8> = vec![0; sample.1];
        let vm = sample.0;
        vm.with_vmcaller(|caller| {
            host.metered_vm_mem_read(caller, vm.clone(), 0, 0..sample.1, &mut buf)
                .unwrap()
        })
    }
}

pub struct VmMemWriteRun;
impl CostRunner for VmMemWriteRun {
    const COST_TYPE: CostType = CostType::VmMemWrite;

    type SampleType = (Rc<Vm>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, mut sample: Self::SampleType) {
        let buf_range = 0..sample.1.len();
        let vm = sample.0;
        vm.with_vmcaller(|caller| {
            host.metered_vm_mem_write(caller, vm.clone(), 0, buf_range, &mut sample.1)
                .unwrap()
        })
    }
}
