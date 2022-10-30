use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::ImVecNewRun,
    xdr::{ScObject, ScVal, ScVec},
    Host,
};

pub(crate) struct ImVecNewMeasure;

/// Measures the costs of allocating 0-sized vectors.
impl HostCostMeasurement for ImVecNewMeasure {
    type Runner = ImVecNewRun;

    // All cases are the random case, which is a constant-effort operation.
    fn new_random_case(_host: &Host, _rng: &mut StdRng, _input: u64) -> ScVal {
        let scvec: ScVec = ScVec(vec![].try_into().unwrap());
        ScVal::Object(Some(ScObject::Vec(scvec)))
    }
}
