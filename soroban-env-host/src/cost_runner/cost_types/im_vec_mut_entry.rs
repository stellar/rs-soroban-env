use super::im_vec_immut_entry::ImVecImmutEntrySample;
use crate::{budget::CostType, cost_runner::CostRunner, EnvVal, Host, RawVal};

pub struct ImVecMutEntryRun;

pub struct ImVecMutEntrySample {
    pub im: ImVecImmutEntrySample,
    pub second_vec_ref: im_rc::Vector<EnvVal<Host, RawVal>>,
}

impl CostRunner for ImVecMutEntryRun {
    const COST_TYPE: CostType = CostType::ImVecMutEntry;
    type SampleType = ImVecMutEntrySample;

    fn run_iter(_host: &Host, iter: u64, sample: &mut Self::SampleType) {
        let _ = sample
            .im
            .vec
            .get_mut(sample.im.idxs[iter as usize % sample.im.idxs.len()]);
    }

    fn get_total_input(_host: &Host, sample: &Self::SampleType) -> u64 {
        (sample.im.vec.len() as u64) * Self::RUN_ITERATIONS
    }
}
