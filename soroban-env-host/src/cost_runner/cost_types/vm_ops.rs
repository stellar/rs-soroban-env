use crate::{budget::CostType, cost_runner::CostRunner, xdr::Hash, Vm};
use std::rc::Rc;

pub struct VmInstantiationRun;

#[derive(Clone)]
pub struct VmInstantiationSample {
    pub id: Hash,
    pub wasm: Vec<u8>,
}

impl VmInstantiationSample {
    pub fn new<I: Into<Vec<u8>>>(id: Hash, wasm: I) -> Self {
        Self {
            id,
            wasm: wasm.into(),
        }
    }

    pub fn default_id<I: Into<Vec<u8>>>(wasm: I) -> Self {
        Self {
            id: [0; 32].into(),
            wasm: wasm.into(),
        }
    }
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
            host.metered_vm_read_bytes_from_linear_memory(caller, &vm, 0, &mut buf)
                .unwrap();
        });
    }
}

pub struct VmMemWriteRun;
impl CostRunner for VmMemWriteRun {
    const COST_TYPE: CostType = CostType::VmMemWrite;

    type SampleType = (Rc<Vm>, Vec<u8>);

    #[allow(clippy::unnecessary_mut_passed)]
    fn run_iter(host: &crate::Host, _iter: u64, mut sample: Self::SampleType) {
        let vm = sample.0;
        vm.with_vmcaller(|caller| {
            host.metered_vm_write_bytes_to_linear_memory(caller, &vm, 0, &mut sample.1)
                .unwrap();
        });
    }
}
