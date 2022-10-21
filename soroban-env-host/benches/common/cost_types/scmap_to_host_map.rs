use std::collections::HashSet;

use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, Rng};
use soroban_env_host::xdr::ScVal;
use soroban_env_host::{
    budget::CostType,
    xdr::{ScMap, ScMapEntry, ScObject},
    Host,
};

pub(crate) struct ScMapToHostMapRun {
    val: ScVal,
    input: u64,
}

// This measures the costs of converting maps of varying sizes
// from XDR to host format. The input is the size of the map,
// the costs should be linear.
impl HostCostMeasurement for ScMapToHostMapRun {
    const COST_TYPE: CostType = CostType::ScMapToHostMap;
    const RUN_ITERATIONS: u64 = 5;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Self {
        let input = input * 100;
        let mut hs: HashSet<u32> = HashSet::new();
        while hs.len() < (input as usize) {
            hs.insert(rng.gen());
        }
        let scmap: ScMap = ScMap(
            hs.iter()
                .map(|i| ScMapEntry {
                    key: ScVal::U32(*i),
                    val: ScVal::U32(*i),
                })
                .collect::<Vec<ScMapEntry>>()
                .try_into()
                .unwrap(),
        );
        let val = ScVal::Object(Some(ScObject::Map(scmap)));
        Self { val, input }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.input
    }

    fn run(&mut self, _iter: u64, host: &Host) {
        host.inject_val(&self.val).unwrap();
    }
}
