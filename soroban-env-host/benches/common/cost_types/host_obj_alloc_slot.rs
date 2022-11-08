use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{
    cost_runner::HostObjAllocSlotRun,
    xdr::{ScObject, ScVal},
    Host,
};

// This measures the cost of allocating a slot in the host object array,
// which is pretty much just the cost of doing a vector-grow operation in
// rust. It should be _amortized_ constant-time. The input value is the
// new object handle number, which is the size of the existing object array.
pub(crate) struct HostObjAllocSlotMeasure;

impl HostCostMeasurement for HostObjAllocSlotMeasure {
    type Runner = HostObjAllocSlotRun;

    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> Vec<u8> {
        // During setup we inject a bunch of copies of the object to make
        // the host object array large.
        let size = input * 100;
        let val = ScVal::Object(Some(ScObject::I64(0)));
        for _ in 0..size {
            host.inject_val(&val).unwrap();
        }
        // here we insert a pre-constructed bytes of various sizes to show that
        // the cost of inserting one additional host object is constant w.r.t.
        // the actual object size.
        (0..input).map(|_| rng.next_u32() as u8).collect()
    }
}
