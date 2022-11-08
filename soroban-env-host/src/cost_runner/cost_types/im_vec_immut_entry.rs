use crate::{budget::CostType, cost_runner::CostRunner, EnvVal, Host, RawVal};

pub struct ImVecImmutEntryRun;

#[derive(Clone)]
pub struct ImVecImmutEntrySample {
    pub vec: im_rc::Vector<EnvVal<Host, RawVal>>,
    pub idxs: Vec<usize>,
}

impl CostRunner for ImVecImmutEntryRun {
    const COST_TYPE: CostType = CostType::ImVecImmutEntry;
    type SampleType = ImVecImmutEntrySample;

    fn run_iter(_host: &Host, iter: u64, sample: Self::SampleType) {
        let _ = sample
            .vec
            .get(sample.idxs[iter as usize % sample.idxs.len()]);
    }

    fn get_total_input(_host: &Host, sample: &Self::SampleType) -> u64 {
        (sample.vec.len() as u64) * Self::RUN_ITERATIONS
    }
}
