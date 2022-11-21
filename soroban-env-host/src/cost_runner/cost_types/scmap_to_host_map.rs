use crate::{budget::CostType, cost_runner::CostRunner, xdr::ScVal};

pub struct ScMapToHostMapRun;

impl CostRunner for ScMapToHostMapRun {
    const COST_TYPE: CostType = CostType::ScMapToHostMap;
    type SampleType = ScVal;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        host.inject_val(&sample).unwrap();
    }
}
