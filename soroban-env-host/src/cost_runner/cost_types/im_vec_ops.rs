use crate::{
    budget::CostType, cost_runner::CostRunner, host::metered_cmp::MeteredCmp, EnvVal, Host,
    MeteredVector, RawVal,
};

type HostVec = MeteredVector<EnvVal<Host, RawVal>>;

pub struct ImVecNewRun;
impl CostRunner for ImVecNewRun {
    const COST_TYPE: CostType = CostType::ImVecNew;
    type SampleType = ();

    fn run_iter(host: &crate::Host, _iter: u64, _sample: Self::SampleType) {
        HostVec::new(host.budget_cloned()).unwrap();
    }
}

pub struct ImVecImmutEntryRun;
#[derive(Clone)]
pub struct ImVecImmutEntrySample {
    pub vec: HostVec,
    pub idxs: Vec<usize>,
}
impl CostRunner for ImVecImmutEntryRun {
    const COST_TYPE: CostType = CostType::ImVecImmutEntry;
    type SampleType = ImVecImmutEntrySample;

    fn run_iter(_host: &Host, iter: u64, sample: Self::SampleType) {
        let _ = sample
            .vec
            .get(sample.idxs[iter as usize % sample.idxs.len()])
            .unwrap();
    }
}

pub struct ImVecMutEntryRun;
#[derive(Clone)]
pub struct ImVecMutEntrySample {
    pub im: ImVecImmutEntrySample,
    pub second_vec_ref: HostVec,
}
impl CostRunner for ImVecMutEntryRun {
    const COST_TYPE: CostType = CostType::ImVecMutEntry;
    type SampleType = ImVecMutEntrySample;

    fn run_iter(_host: &Host, iter: u64, mut sample: Self::SampleType) {
        let _ = sample
            .im
            .vec
            .get_mut(sample.im.idxs[iter as usize % sample.im.idxs.len()])
            .unwrap();
    }
}

pub struct ImVecCmpRun;
#[derive(Clone)]
pub struct ImVecCmpSample {
    pub a: HostVec,
    pub b: HostVec,
}
impl CostRunner for ImVecCmpRun {
    const COST_TYPE: CostType = CostType::ImVecCmp;
    type SampleType = ImVecCmpSample;

    fn run_iter(host: &Host, _iter: u64, sample: Self::SampleType) {
        sample
            .a
            .metered_cmp(&sample.b, &host.budget_cloned())
            .unwrap();
    }
}
