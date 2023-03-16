use soroban_env_common::Compare;

use crate::{
    budget::{AsBudget, CostType},
    cost_runner::CostRunner,
};

pub struct HostMemCmpRun;
impl CostRunner for HostMemCmpRun {
    const COST_TYPE: CostType = CostType::HostMemCmp;
    type SampleType = (Vec<u8>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        host.as_budget()
            .compare(&sample.0.as_slice(), &sample.1.as_slice())
            .unwrap();
        // `forget` avoids deallocation of sample which artificially inflates the cost
        std::mem::forget(sample)
    }
}
