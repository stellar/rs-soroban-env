use crate::{budget::CostType, cost_runner::CostRunner, xdr::Hash, Vm};
use std::rc::Rc;

pub struct VmInstantiationRun;

#[derive(Clone)]
pub struct VmInstantiationSample {
    pub id: Option<Hash>,
    pub wasm: Vec<u8>,
}

impl CostRunner for VmInstantiationRun {
    const COST_TYPE: CostType = CostType::VmInstantiation;

    const RUN_ITERATIONS: u64 = 10;

    type SampleType = VmInstantiationSample;

    type RecycledType = (Option<Rc<Vm>>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let vm = Vm::new(host, sample.id.unwrap(), &sample.wasm[..]).unwrap();
        (Some(vm), sample.wasm)
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        (None, sample.wasm)
    }
}

pub struct VmMemReadRun;
impl CostRunner for VmMemReadRun {
    const COST_TYPE: CostType = CostType::VmMemRead;

    type SampleType = (Rc<Vm>, Vec<u8>);

    type RecycledType = Self::SampleType;

    fn run_iter(
        host: &crate::Host,
        _iter: u64,
        mut sample: Self::SampleType,
    ) -> Self::RecycledType {
        let vm = &sample.0;
        vm.with_vmcaller(|caller| {
            host.metered_vm_read_bytes_from_linear_memory(caller, vm, 0, &mut sample.1)
                .unwrap()
        });
        sample
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        sample
    }
}

pub struct VmMemWriteRun;
impl CostRunner for VmMemWriteRun {
    const COST_TYPE: CostType = CostType::VmMemWrite;

    type SampleType = (Rc<Vm>, Vec<u8>);

    type RecycledType = Self::SampleType;

    fn run_iter(
        host: &crate::Host,
        _iter: u64,
        mut sample: Self::SampleType,
    ) -> Self::RecycledType {
        let vm = &sample.0;
        vm.with_vmcaller(|caller| {
            host.metered_vm_write_bytes_to_linear_memory(caller, vm, 0, &mut sample.1)
                .unwrap()
        });
        sample
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        sample
    }
}
