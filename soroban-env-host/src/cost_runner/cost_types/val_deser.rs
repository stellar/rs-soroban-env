use crate::{budget::CostType, cost_runner::CostRunner, xdr::ScVal};

pub struct ValDeserRun;

impl CostRunner for ValDeserRun {
    const COST_TYPE: CostType = CostType::ValDeser;
    type SampleType = Vec<u8>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        host.metered_from_xdr::<ScVal>(&sample).unwrap();
    }
}
