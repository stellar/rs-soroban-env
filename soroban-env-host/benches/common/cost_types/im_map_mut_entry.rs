use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::{ImMapMutEntryRun, ImMapMutEntrySample},
    Host,
};

use super::im_map_immut_entry::ImMapImmutEntryMeasure;

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
