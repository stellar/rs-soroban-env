use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, seq::SliceRandom};
use soroban_env_host::{budget::CostType, EnvVal, Host, RawVal};

use super::util;

pub(crate) struct ImMapImmutEntryRun {
    pub(crate) map: im_rc::OrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>>,
    pub(crate) keys: Vec<EnvVal<Host, RawVal>>,
}

// Measures the costs of accessing maps of varying sizes. It should have zero
// memory cost and logarithmic cpu cost, which we will approximate as constant
// with an upper bound on map size. The input value is the size of the map.
impl HostCostMeasurement for ImMapImmutEntryRun {
    const RUN_ITERATIONS: u64 = 100;
    const COST_TYPE: CostType = CostType::ImMapImmutEntry;

    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> Self {
        let input = input * 100;
        let mut keys: Vec<_> = util::to_envval_u32(host, 0..(input as u32)).collect();
        keys.shuffle(rng);
        let map = keys.iter().cloned().zip(keys.iter().cloned()).collect();
        keys.shuffle(rng);
        Self { map, keys }
    }

    fn new_worst_case(host: &Host, _rng: &mut StdRng, input: u64) -> Self {
        let input = input * 100;
        let keys: Vec<_> = util::to_envval_u32(host, 0..(input as u32)).collect();
        let map = keys.iter().cloned().zip(keys.iter().cloned()).collect();
        let keys = util::to_envval_u32(host, [0, u32::MAX].iter().cloned()).collect();
        Self { map, keys }
    }

    fn new_best_case(host: &Host, _rng: &mut StdRng) -> Self {
        let keys: Vec<_> = util::to_envval_u32(host, [0].iter().cloned()).collect();
        let map = keys.iter().cloned().zip(keys.iter().cloned()).collect();
        Self { map, keys }
    }

    fn run(&mut self, iter: u64, _host: &Host) {
        let _ = self.map.get(&self.keys[iter as usize % self.keys.len()]);
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.map.len() as u64
    }
}
