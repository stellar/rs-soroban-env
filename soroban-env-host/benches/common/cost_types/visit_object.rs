use crate::common::HostCostMeasurement;
use soroban_env_host::{
    cost_runner::VisitObjectRun,
    xdr::{ScObject, ScVal},
    Host, RawVal,
};

pub(crate) struct VisitObjectMeasure;

impl HostCostMeasurement for VisitObjectMeasure {
    type Runner = VisitObjectRun;

    fn new_random_case(host: &Host, _rng: &mut rand::prelude::StdRng, input: u64) -> Vec<RawVal> {
        // During setup we inject a bunch of copies of the object to make
        // the host object array large.
        let size = 1 + input * 100;
        let mut vec = Vec::with_capacity(size as usize);
        let val = ScVal::Object(Some(ScObject::I64(0)));
        for _ in 0..size {
            vec.push(host.inject_val(&val).unwrap());
        }
        vec
    }
}
