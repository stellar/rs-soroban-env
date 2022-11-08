use crate::{budget::CostType, cost_runner::CostRunner, xdr::ScVal, RawVal};

pub struct ValXdrConvRun;

impl CostRunner for ValXdrConvRun {
    const COST_TYPE: CostType = CostType::ValXdrConv;
    type SampleType = (RawVal, ScVal);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        host.from_host_val(sample.0).unwrap();
        host.to_host_val(&sample.1).unwrap();
    }
}
