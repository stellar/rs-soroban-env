use std::collections::HashSet;

use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, Rng};
use soroban_env_host::{
    cost_runner::ValSerRun,
    xdr::{ScMap, ScMapEntry, ScVal},
    Host,
};

pub(crate) struct ValSerMeasure;

// This measures the costs of converting an ScVal into bytes.
impl HostCostMeasurement for ValSerMeasure {
    type Runner = ValSerRun;

    const STEP_SIZE: u64 = 100;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> (ScVal, Vec<u8>) {
        let input = 1 + input * Self::STEP_SIZE;
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
        (ScVal::Map(Some(scmap)), Vec::default())
    }
}
