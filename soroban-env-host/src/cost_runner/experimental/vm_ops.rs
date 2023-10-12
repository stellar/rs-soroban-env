use crate::{cost_runner::CostRunner, xdr::ContractCostType, xdr::Hash, Vm};
use std::{hint::black_box, rc::Rc};

#[derive(Clone)]
pub struct VmMemRunSample {
    pub vm: Rc<Vm>,
    pub buf: Vec<u8>,
}

pub struct VmMemReadRun;
impl CostRunner for VmMemReadRun {
    const COST_TYPE: ContractCostType = ContractCostType::VmMemRead;

    type SampleType = VmMemRunSample;

    type RecycledType = Self::SampleType;

    fn run_iter(
        host: &crate::Host,
        _iter: u64,
        mut sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(
            sample
                .vm
                .with_vmcaller(|caller| {
                    host.metered_vm_read_bytes_from_linear_memory(
                        caller,
                        &sample.vm,
                        0,
                        &mut sample.buf,
                    )
                })
                .unwrap(),
        );
        sample
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, Some(0)).unwrap());
        black_box(sample)
    }
}

pub struct VmMemWriteRun;
impl CostRunner for VmMemWriteRun {
    const COST_TYPE: ContractCostType = ContractCostType::VmMemWrite;

    type SampleType = VmMemRunSample;

    type RecycledType = Self::SampleType;

    fn run_iter(
        host: &crate::Host,
        _iter: u64,
        mut sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(
            sample
                .vm
                .with_vmcaller(|caller| {
                    host.metered_vm_write_bytes_to_linear_memory(
                        caller,
                        &sample.vm,
                        0,
                        &mut sample.buf,
                    )
                })
                .unwrap(),
        );
        sample
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, Some(0)).unwrap());
        black_box(sample)
    }
}
