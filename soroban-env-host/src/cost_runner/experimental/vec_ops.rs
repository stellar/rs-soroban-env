use std::hint::black_box;

use crate::budget::AsBudget;
use crate::{cost_runner::CostRunner, xdr::ContractCostType, Host, MeteredVector, Val};

type HostVec = MeteredVector<Val>;

pub struct VecEntryRun;

#[derive(Clone)]
pub struct VecEntrySample {
    pub vec: HostVec,
    pub idxs: Vec<usize>,
}
impl CostRunner for VecEntryRun {
    const COST_TYPE: ContractCostType = ContractCostType::VecEntry;

    const RUN_ITERATIONS: u64 = 1000;

    type SampleType = VecEntrySample;

    type RecycledType = VecEntrySample;

    fn run_iter(host: &Host, iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let _ = black_box(
            sample
                .vec
                .get(
                    sample.idxs[iter as usize % sample.idxs.len()],
                    host.as_budget(),
                )
                .unwrap()
                .get_payload(),
        );
        black_box(sample)
    }

    fn run_baseline_iter(host: &Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, None).unwrap());
        black_box(sample)
    }
}
