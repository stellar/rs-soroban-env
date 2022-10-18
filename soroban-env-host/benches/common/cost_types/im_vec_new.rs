use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    budget::CostType,
    xdr::{ScObject, ScVal, ScVec},
    Host,
};

pub(crate) struct ImVecNewRun {
    val: ScVal,
}

/// Measures the costs of allocating 0-sized vectors.
impl HostCostMeasurement for ImVecNewRun {
    const COST_TYPE: CostType = CostType::ImVecNew;
    const RUN_ITERATIONS: u64 = 1000;

    // All cases are the random case, which is a constant-effort operation.
    fn new_random_case(_host: &Host, _rng: &mut StdRng, _input: u64) -> Self {
        let scvec: ScVec = ScVec(vec![].try_into().unwrap());
        let val = ScVal::Object(Some(ScObject::Vec(scvec)));
        Self { val }
    }

    fn run(&mut self, _iter: u64, host: &Host) {
        host.inject_val(&self.val).unwrap();
    }
}
