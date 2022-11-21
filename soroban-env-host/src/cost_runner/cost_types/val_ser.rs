use crate::{budget::CostType, cost_runner::CostRunner, xdr::ScVal};

pub struct ValSerRun;

impl CostRunner for ValSerRun {
    const COST_TYPE: CostType = CostType::ValSer;
    type SampleType = ScVal;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        let mut buf = Vec::<u8>::new();
        host.metered_write_xdr(&sample, &mut buf).unwrap()
    }
}
