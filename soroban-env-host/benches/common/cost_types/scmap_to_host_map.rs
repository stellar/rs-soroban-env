use std::collections::BTreeSet;

use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, Rng};
use soroban_env_host::{
    cost_runner::ScMapToHostMapRun,
    xdr::{ScMap, ScMapEntry, ScObject, ScVal},
    Host,
};

pub(crate) struct ScMapToHostMapMeasure;

// This measures the costs of converting maps of varying sizes
// from XDR to host format. The input is the size of the map,
// the costs should be linear.
impl HostCostMeasurement for ScMapToHostMapMeasure {
    type Runner = ScMapToHostMapRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> ScVal {
        let input = input * 100;
        let mut hs: BTreeSet<u32> = BTreeSet::new();
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
        ScVal::Object(Some(ScObject::Map(scmap)))
    }
}
