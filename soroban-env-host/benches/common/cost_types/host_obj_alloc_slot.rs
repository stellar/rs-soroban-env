use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    budget::CostType,
    xdr::{ScObject, ScVal},
    Host,
};

// This measures the cost of allocating a slot in the host object array,
// which is pretty much just the cost of doing a vector-grow operation in
// rust. It should be _amortized_ constant-time. The input value is the
// new object handle number, which is the size of the existing object array.
pub(crate) struct HostObjAllocSlotRun {
    val: ScVal,
}

impl HostCostMeasurement for HostObjAllocSlotRun {
    const COST_TYPE: CostType = CostType::HostObjAllocSlot;

    // We iterate a bunch of times so that we cross a reallocation boundary.
    const RUN_ITERATIONS: u64 = 100;

    fn new_random_case(host: &Host, _rng: &mut StdRng, input: u64) -> Self {
        // During setup we inject a bunch of copies of the object to make
        // the host object array large.
        let size = input * 100;
        let val = ScVal::Object(Some(ScObject::I64(0)));
        for _ in 0..size {
            host.inject_val(&val).unwrap();
        }
        Self { val }
    }

    fn run(&mut self, _iter: u64, host: &Host) {
        // When measuring, we just inject a single copy to see what
        // the cost of "one more" is at the given size.
        host.inject_val(&self.val).unwrap();
    }
}
