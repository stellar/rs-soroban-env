use soroban_env_common::xdr::{BytesM, ScBytes};

use crate::{budget::CostType, cost_runner::CostRunner};

pub struct HostObjAllocSlotRun;

impl CostRunner for HostObjAllocSlotRun {
    const COST_TYPE: CostType = CostType::HostObjAllocSlot;
    type SampleType = Vec<u8>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        // When measuring, we just inject a single copy to see what
        // the cost of "one more" is at the given size.
        // host.to_host_obj(sample).unwrap();
        host.add_host_object(ScBytes(BytesM::try_from(sample).unwrap()))
            .unwrap();
    }
}
