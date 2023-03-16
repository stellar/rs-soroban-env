use crate::budget::AsBudget;
use crate::{budget::CostType, cost_runner::CostRunner, Host, MeteredVector, RawVal};

type HostVec = MeteredVector<RawVal>;

pub struct ImVecNewRun;
impl CostRunner for ImVecNewRun {
    const COST_TYPE: CostType = CostType::VecNew;
    type SampleType = Vec<RawVal>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        // `forget` avoids deallocation of sample which artificially inflates the cost
        std::mem::forget(HostVec::from_vec(sample, host.as_budget()).unwrap());
    }
}

pub struct ImVecEntryRun;
#[derive(Clone)]
pub struct ImVecEntrySample {
    pub vec: HostVec,
    pub idxs: Vec<usize>,
}
impl CostRunner for ImVecEntryRun {
    const COST_TYPE: CostType = CostType::VecEntry;
    type SampleType = ImVecEntrySample;

    fn run_iter(host: &Host, _iter: u64, sample: Self::SampleType) {
        for i in &sample.idxs {
            sample.vec.get(*i, host.as_budget()).unwrap();
        }
        // `forget` avoids deallocation of sample which artificially inflates the cost
        std::mem::forget(sample)
    }
}
