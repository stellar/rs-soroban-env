use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::xdr::ScVal;
use soroban_env_host::{
    budget::CostType,
    xdr::{ScObject, ScVec},
    Host,
};

pub(crate) struct ScVecToHostVecRun {
    input: u64,
    val: ScVal,
}

// This measures the costs of converting vectors of varying sizes from XDR to
// host format. The input is the size of the map, the costs should be linear.
impl HostCostMeasurement for ScVecToHostVecRun {
    const COST_TYPE: CostType = CostType::ScVecToHostVec;
    const RUN_ITERATIONS: u64 = 5;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, input: u64) -> Self {
        let input = input * 100;
        let scvec: ScVec = ScVec(
            (0..input)
                .map(|i| ScVal::U32(i as u32))
                .collect::<Vec<ScVal>>()
                .try_into()
                .unwrap(),
        );
        let val = ScVal::Object(Some(ScObject::Vec(scvec)));
        Self { input, val }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.input
    }

    fn run(&mut self, _iter: u64, host: &Host) {
        host.inject_val(&self.val).unwrap();
    }
}
