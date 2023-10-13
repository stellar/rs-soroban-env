use std::hint::black_box;

use crate::{
    cost_runner::{CostRunner, CostType},
    xdr::ContractCostType::MemCpy,
};

pub struct MemCpyRun;

impl CostRunner for MemCpyRun {
    const COST_TYPE: CostType = CostType::Contract(MemCpy);

    type SampleType = (Vec<u8>, Vec<u8>);

    type RecycledType = Self::SampleType;

    fn run_iter(host: &crate::Host, _iter: u64, mut sample: Self::SampleType) -> Self::SampleType {
        black_box(
            host.mem_copy_from_slice(sample.0.as_slice(), sample.1.as_mut_slice())
                .unwrap(),
        );
        sample
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(MemCpy, Some(0)).unwrap());
        black_box(sample)
    }
}
