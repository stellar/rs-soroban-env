use crate::common::{util, HostCostMeasurement};
use rand::{rngs::StdRng, seq::SliceRandom};
use soroban_env_host::{
    cost_runner::{ImMapImmutEntryRun, ImMapImmutEntrySample},
    Host,
};

pub(crate) struct ImMapImmutEntryMeasure;
// Measures the costs of accessing maps of varying sizes. It should have zero
// memory cost and logarithmic cpu cost, which we will approximate as constant
// with an upper bound on map size. The input value is the size of the map.
impl HostCostMeasurement for ImMapImmutEntryMeasure {
    type Runner = ImMapImmutEntryRun;

    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> ImMapImmutEntrySample {
        let input = input * 100;
        let mut keys: Vec<_> = util::to_envval_u32(host, 0..(input as u32)).collect();
        keys.shuffle(rng);
        let map = keys.iter().cloned().zip(keys.iter().cloned()).collect();
        keys.shuffle(rng);
        ImMapImmutEntrySample { map, keys }
    }

    fn new_worst_case(host: &Host, _rng: &mut StdRng, input: u64) -> ImMapImmutEntrySample {
        let input = input * 100;
        let keys: Vec<_> = util::to_envval_u32(host, 0..(input as u32)).collect();
        let map = keys.iter().cloned().zip(keys.iter().cloned()).collect();
        let keys = util::to_envval_u32(host, [0, u32::MAX].iter().cloned()).collect();
        ImMapImmutEntrySample { map, keys }
    }

    fn new_best_case(host: &Host, _rng: &mut StdRng) -> ImMapImmutEntrySample {
        let keys: Vec<_> = util::to_envval_u32(host, [0].iter().cloned()).collect();
        let map = keys.iter().cloned().zip(keys.iter().cloned()).collect();
        ImMapImmutEntrySample { map, keys }
    }
}
