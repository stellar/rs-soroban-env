use std::hint::black_box;

use crate::{
    cost_runner::{CostRunner, CostType},
    xdr::ContractCostType::ValDeser,
    xdr::ScVal,
};

pub struct ValDeserRun;

impl CostRunner for ValDeserRun {
    const COST_TYPE: CostType = CostType::Contract(ValDeser);

    type SampleType = Vec<u8>;

    type RecycledType = (Option<ScVal>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let sv = black_box(host.metered_from_xdr::<ScVal>(&sample).unwrap());
        (Some(sv), sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(ValDeser, Some(0)).unwrap());
        black_box((None, sample))
    }
}
