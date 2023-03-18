use std::hint::black_box;

use crate::{budget::CostType, cost_runner::CostRunner, host_object::HostObject, Object};

pub struct VisitObjectRun;

impl CostRunner for VisitObjectRun {
    const COST_TYPE: CostType = CostType::VisitObject;

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
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box((None, sample))
    }
}
