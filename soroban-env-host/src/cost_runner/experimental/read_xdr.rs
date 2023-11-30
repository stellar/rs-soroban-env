use std::hint::black_box;

use crate::{
    budget::{AsBudget, CostTracker},
    cost_runner::{experimental::ExperimentalCostType::ReadXdrByteArray, CostRunner, CostType},
    xdr::{ContractCostType::ValDeser, ScVal},
};

pub struct ReadXdrByteArrayRun;

impl CostRunner for ReadXdrByteArrayRun {
    // experimental cost type that identifies this component. Used purely for result aggregation
    // purpose. Internally there is no budget component associated with this type, and you'll
    // have to override the get_tracker method to return the correct inputs.
    const COST_TYPE: CostType = CostType::Experimental(ReadXdrByteArray);

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

    fn get_tracker(host: &crate::Host) -> CostTracker {
        // internally this is still charged under `ValDeser`
        host.as_budget().get_tracker(ValDeser).unwrap()
    }
}
