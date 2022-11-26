use crate::budget::AsBudget;
use crate::{budget::CostType, cost_runner::CostRunner, Host, MeteredVector, RawVal};

type HostVec = MeteredVector<RawVal>;

pub struct ImVecNewRun;
impl CostRunner for ImVecNewRun {
    const COST_TYPE: CostType = CostType::VecNew;
    type SampleType = ();

    fn run_iter(host: &crate::Host, _iter: u64, _sample: Self::SampleType) {
        HostVec::new(host.as_budget()).unwrap();
    }
}

pub struct ImVecImmutEntryRun;
#[derive(Clone)]
pub struct ImVecImmutEntrySample {
    pub vec: HostVec,
    pub idxs: Vec<usize>,
}
impl CostRunner for ImVecImmutEntryRun {
    const COST_TYPE: CostType = CostType::VecEntry;
    type SampleType = ImVecImmutEntrySample;

    fn run_iter(host: &Host, iter: u64, sample: Self::SampleType) {
        let _ = sample
            .vec
            .get(
                sample.idxs[iter as usize % sample.idxs.len()],
                host.as_budget(),
            )
            .unwrap();
    }
}
