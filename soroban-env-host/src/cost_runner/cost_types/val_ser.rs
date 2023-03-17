use crate::{budget::CostType, cost_runner::CostRunner, xdr::ScVal};

pub struct ValSerRun;

impl CostRunner for ValSerRun {
    const COST_TYPE: CostType = CostType::ValSer;

    type SampleType = (ScVal, Vec<u8>);

    type RecycledType = Self::SampleType;

    fn run_iter(
        host: &crate::Host,
        _iter: u64,
        mut sample: Self::SampleType,
    ) -> Self::RecycledType {
        // Note the sample.1 is an empty vector, so metered_write_xdr includes allocation
        // cost. This is how its typically used so we are setting it up this way.
        host.metered_write_xdr(&sample.0, &mut sample.1).unwrap();
        sample
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        sample
    }
}
