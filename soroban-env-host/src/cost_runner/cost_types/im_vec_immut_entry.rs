use crate::{budget::CostType, cost_runner::CostRunner, EnvVal, Host, RawVal};

pub struct ImVecImmutEntryRun;

pub struct ImVecImmutEntrySample {
    pub vec: im_rc::Vector<EnvVal<Host, RawVal>>,
    pub idxs: Vec<usize>,
}

impl CostRunner for ImVecImmutEntryRun {
    const COST_TYPE: CostType = CostType::ImVecImmutEntry;
    type SampleType = ImVecImmutEntrySample;

    fn run_iter(_host: &Host, iter: u64, sample: &mut Self::SampleType) -> Option<u64> {
        let _ = sample
            .vec
            .get(sample.idxs[iter as usize % sample.idxs.len()]);
        Some(sample.vec.len() as u64)
    }
}
