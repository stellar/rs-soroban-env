use std::hint::black_box;

use crate::{cost_runner::CostRunner, host_object::HostObject, xdr::ContractCostType, Object};

pub struct VisitObjectRun;

impl CostRunner for VisitObjectRun {
    const COST_TYPE: ContractCostType = ContractCostType::VisitObject;

    const RUN_ITERATIONS: u64 = 1000;

    type SampleType = Vec<Object>;

    type RecycledType = Self::SampleType;

    fn run_iter(host: &crate::Host, iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        unsafe {
            let _ = black_box(
                host.unchecked_visit_val_obj(sample[iter as usize % sample.len()], |obj| match obj
                    .unwrap()
                {
                    HostObject::I64(i) => Ok(*i),
                    _ => panic!("unexpected type, check HCM"),
                })
                .unwrap(),
            );
            black_box(sample)
        }
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, None).unwrap());
        black_box(sample)
    }
}
