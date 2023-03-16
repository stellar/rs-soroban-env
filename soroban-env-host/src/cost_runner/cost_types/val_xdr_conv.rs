use crate::{budget::CostType, cost_runner::CostRunner, xdr::ScVal, RawVal};

pub struct ValXdrConvRun;

impl CostRunner for ValXdrConvRun {
    const COST_TYPE: CostType = CostType::ValXdrConv;
    type SampleType = (RawVal, ScVal);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        // Here we don't need to `forget` the sample since its size is fixed (a single shallow Val).
        // The deallocation has been accounted for by the baseline measurement.
        host.from_host_val(sample.0).unwrap();
        host.to_host_val(&sample.1).unwrap();
    }
}
