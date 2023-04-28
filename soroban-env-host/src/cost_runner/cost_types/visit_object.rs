use std::hint::black_box;

use crate::{cost_runner::CostRunner, host_object::HostObject, xdr::ContractCostType, Object};

pub struct VisitObjectRun;

impl CostRunner for VisitObjectRun {
    const COST_TYPE: ContractCostType = ContractCostType::VisitObject;

    type SampleType = Vec<Object>;

    type RecycledType = (Option<i64>, Self::SampleType);

    fn run_iter(host: &crate::Host, iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        unsafe {
            let i = black_box(
                host.unchecked_visit_val_obj(sample[iter as usize % sample.len()], |obj| match obj
                    .unwrap()
                {
                    HostObject::I64(i) => Ok(*i),
                    _ => panic!("unexpected type, check HCM"),
                })
                .unwrap(),
            );
            (Some(i), sample)
        }
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, None).unwrap());
        black_box((None, sample))
    }
}
