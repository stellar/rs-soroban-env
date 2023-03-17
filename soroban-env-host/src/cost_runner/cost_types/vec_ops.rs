use crate::budget::AsBudget;
use crate::{budget::CostType, cost_runner::CostRunner, Host, MeteredVector, RawVal};

type HostVec = MeteredVector<RawVal>;

pub struct VecNewRun;
impl CostRunner for VecNewRun {
    const COST_TYPE: CostType = CostType::VecNew;

    type SampleType = Vec<RawVal>;

    type RecycledType = Option<HostVec>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let hv = HostVec::from_vec(sample, host.as_budget()).unwrap();
        Some(hv)
    }

    fn run_baseline_iter(
        _host: &Host,
        _iter: u64,
        _sample: Self::SampleType,
    ) -> Self::RecycledType {
        None
    }
}

pub struct VecEntryRun;

#[derive(Clone)]
pub struct VecEntrySample {
    pub vec: HostVec,
    pub idxs: Vec<usize>,
}
impl CostRunner for VecEntryRun {
    const COST_TYPE: CostType = CostType::VecEntry;

    type SampleType = VecEntrySample;

    type RecycledType = (Option<u64>, Self::SampleType);

    fn run_iter(host: &Host, iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let v = sample
            .vec
            .get(
                sample.idxs[iter as usize % sample.idxs.len()],
                host.as_budget(),
            )
            .unwrap()
            .get_payload();
        (Some(v), sample)
    }

    fn run_baseline_iter(_host: &Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        (None, sample)
    }
}
