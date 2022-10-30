use crate::{budget::CostType, cost_runner::CostRunner, xdr::ScVal};

pub struct ScVecToHostVecRun;

impl CostRunner for ScVecToHostVecRun {
    const COST_TYPE: CostType = CostType::ScVecToHostVec;
    type SampleType = ScVal;

    fn run_iter(host: &crate::Host, _iter: u64, sample: &mut Self::SampleType) -> Option<u64> {
        host.inject_val(&sample).unwrap();
        None
    }
}
