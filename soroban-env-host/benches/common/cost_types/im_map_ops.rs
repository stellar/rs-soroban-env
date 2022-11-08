use crate::common::{util, HostCostMeasurement};
use rand::{rngs::StdRng, seq::SliceRandom};
use soroban_env_host::{
    cost_runner::{
        ImMapImmutEntryRun, ImMapImmutEntrySample, ImMapMutEntryRun, ImMapMutEntrySample,
        ImMapNewRun,
    },
    Host, MeteredOrdMap,
};

pub(crate) struct ImMapNewMeasure;
// Measures the overhead cost of creating a new map, without inserting into the host storage.
impl HostCostMeasurement for ImMapNewMeasure {
    type Runner = ImMapNewRun;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, _input: u64) {
        ()
    }
}

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
        let om = keys.iter().cloned().zip(keys.iter().cloned()).collect();
        let map: MeteredOrdMap<_, _> = MeteredOrdMap::from_map(host.budget_cloned(), om).unwrap();
        keys.shuffle(rng);
        ImMapImmutEntrySample { map, keys }
    }

    fn new_worst_case(host: &Host, _rng: &mut StdRng, input: u64) -> ImMapImmutEntrySample {
        let input = input * 100;
        let keys: Vec<_> = util::to_envval_u32(host, 0..(input as u32)).collect();
        let om = keys.iter().cloned().zip(keys.iter().cloned()).collect();
        let map: MeteredOrdMap<_, _> = MeteredOrdMap::from_map(host.budget_cloned(), om).unwrap();
        let keys = util::to_envval_u32(host, [0, u32::MAX].iter().cloned()).collect();
        ImMapImmutEntrySample { map, keys }
    }

    fn new_best_case(host: &Host, _rng: &mut StdRng) -> ImMapImmutEntrySample {
        let keys: Vec<_> = util::to_envval_u32(host, [0].iter().cloned()).collect();
        let om = keys.iter().cloned().zip(keys.iter().cloned()).collect();
        let map: MeteredOrdMap<_, _> = MeteredOrdMap::from_map(host.budget_cloned(), om).unwrap();
        ImMapImmutEntrySample { map, keys }
    }
}

pub(crate) struct ImMapMutEntryMeasure;
// This is just a variant of ImMapImmutEntryRun that calls the get_mut method on
// a multiply-referenced map, causing a copy-on-write of some nodes. It should
// cost a nearly-constant (perhaps very-slow-log) amount of memory and CPU.
impl HostCostMeasurement for ImMapMutEntryMeasure {
    type Runner = ImMapMutEntryRun;

    fn new_best_case(host: &Host, rng: &mut StdRng) -> ImMapMutEntrySample {
        let im = ImMapImmutEntryMeasure::new_best_case(host, rng);
        let second_map_ref = im.map.clone();
        ImMapMutEntrySample { im, second_map_ref }
    }

    fn new_worst_case(host: &Host, rng: &mut StdRng, input: u64) -> ImMapMutEntrySample {
        let im = ImMapImmutEntryMeasure::new_worst_case(host, rng, input);
        let second_map_ref = im.map.clone();
        ImMapMutEntrySample { im, second_map_ref }
    }

    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> ImMapMutEntrySample {
        let im = ImMapImmutEntryMeasure::new_random_case(host, rng, input);
        let second_map_ref = im.map.clone();
        ImMapMutEntrySample { im, second_map_ref }
    }
}
