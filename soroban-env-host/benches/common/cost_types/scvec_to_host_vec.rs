use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::ScVecToHostVecRun,
    xdr::{ScObject, ScVal, ScVec},
    Host,
};

pub(crate) struct ScVecToHostVecMeasure;

// This measures the costs of converting vectors of varying sizes from XDR to
// host format. The input is the size of the map, the costs should be linear.
impl HostCostMeasurement for ScVecToHostVecMeasure {
    type Runner = ScVecToHostVecRun;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, input: u64) -> ScVal {
        let input = input * 100;
        let scvec: ScVec = ScVec(
            (0..input)
                .map(|i| ScVal::U32(i as u32))
                .collect::<Vec<ScVal>>()
                .try_into()
                .unwrap(),
        );
        ScVal::Object(Some(ScObject::Vec(scvec)))
    }
}
