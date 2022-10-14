use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    budget::CostType,
    xdr::{ScObject, ScVal},
    Host,
};

pub(crate) struct HostObjAllocSlotRun {
    count: u64,
    val: ScVal,
}

/// Measures the costs of allocating large numbers of simple objects.
impl HostCostMeasurement for HostObjAllocSlotRun {
    const COST_TYPE: CostType = CostType::HostObjAllocSlot;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, input: u64) -> Self {
        let size = input * 100;
        let val = ScVal::Object(Some(ScObject::I64(0)));
        Self { count: size, val }
    }

    fn run(&mut self, _iter: u64, host: &Host) {
        for _ in 0..self.count {
            host.inject_val(&self.val).unwrap();
        }
    }
}
