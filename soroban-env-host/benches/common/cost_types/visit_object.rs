use crate::common::HostCostMeasurement;
use soroban_env_host::{cost_runner::VisitObjectRun, xdr::ScVal, Host, Object, Val};

pub(crate) struct VisitObjectMeasure;

impl HostCostMeasurement for VisitObjectMeasure {
    type Runner = VisitObjectRun;

    fn new_random_case(host: &Host, _rng: &mut rand::prelude::StdRng, input: u64) -> Vec<Object> {
        // During setup we inject a bunch of copies of the object to make
        // the host object array large.
        let size = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        let mut vec: Vec<Object> = Vec::with_capacity(size as usize);
        let val = ScVal::I64(i64::MAX);
        for _ in 0..size {
            let rv: Val = host.inject_val(&val).unwrap();
            vec.push(rv.try_into().unwrap());
        }
        vec
    }
}
