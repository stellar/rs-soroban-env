use crate::{budget::CostType, cost_runner::CostRunner, xdr::ScVal};

pub struct HostObjAllocSlotRun;

impl CostRunner for HostObjAllocSlotRun {
    const COST_TYPE: CostType = CostType::HostObjAllocSlot;
    type SampleType = ScVal;

    fn run_iter(host: &crate::Host, _iter: u64, sample: &mut Self::SampleType) {
        // When measuring, we just inject a single copy to see what
        // the cost of "one more" is at the given size.
        host.inject_val(sample).unwrap();
    }
}
