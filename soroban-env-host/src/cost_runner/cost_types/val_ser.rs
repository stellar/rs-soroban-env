use std::hint::black_box;

use crate::{cost_runner::CostRunner, xdr::ContractCostType, xdr::ScVal};

pub struct ValSerRun;

impl CostRunner for ValSerRun {
    const COST_TYPE: ContractCostType = ContractCostType::ValSer;

    type SampleType = (ScVal, Vec<u8>);

    type RecycledType = Self::SampleType;

    fn run_iter(
        host: &crate::Host,
        _iter: u64,
        mut sample: Self::SampleType,
    ) -> Self::RecycledType {
        // Note the sample.1 is an empty vector, so metered_write_xdr includes allocation
        // cost. This is how it's typically used so we are setting it up this way.
        black_box(host.metered_write_xdr(&sample.0, &mut sample.1).unwrap());
        sample
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, Some(0)).unwrap());
        black_box(sample)
    }
}
