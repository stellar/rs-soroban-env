use crate::{budget::CostType, cost_runner::CostRunner, Object};

pub struct VisitObjectRun;

impl CostRunner for VisitObjectRun {
    const COST_TYPE: CostType = CostType::VisitObject;
    type SampleType = Vec<Object>;

    fn run_iter(host: &crate::Host, iter: u64, sample: Self::SampleType) {
        unsafe {
            let obj = sample[iter as usize % sample.len()];
            let _ = host.unchecked_visit_val_obj(obj, |_| Ok(()));
        }
    }
}
