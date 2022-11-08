use crate::{budget::CostType, cost_runner::CostRunner, xdr::ScVal};

pub struct ImVecNewRun;

impl CostRunner for ImVecNewRun {
    const COST_TYPE: CostType = CostType::ImVecNew;
    type SampleType = ScVal;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        // TODO: this is incorrect, the ImVecNew does not account for injecting to host.
        host.inject_val(&sample).unwrap();
    }
}
