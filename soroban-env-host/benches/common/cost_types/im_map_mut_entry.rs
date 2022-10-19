use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{budget::CostType, EnvVal, Host, RawVal};

use super::im_map_immut_entry::ImMapImmutEntryRun;

pub(crate) struct ImMapMutEntryRun {
    im: ImMapImmutEntryRun,
    second_map_ref: im_rc::OrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>>,
}

// This is just a variant of ImMapImmutEntryRun that calls the get_mut method on
// a multiply-referenced map, causing a copy-on-write of some nodes. It should
// cost a nearly-constant (perhaps very-slow-log) amount of memory and CPU.
impl HostCostMeasurement for ImMapMutEntryRun {
    const COST_TYPE: CostType = CostType::ImMapMutEntry;
    const RUN_ITERATIONS: u64 = ImMapImmutEntryRun::RUN_ITERATIONS;

    fn new_best_case(host: &Host, rng: &mut StdRng) -> Self {
        let im = ImMapImmutEntryRun::new_best_case(host, rng);
        let second_map_ref = im.map.clone();
        Self { im, second_map_ref }
    }

    fn new_worst_case(host: &Host, rng: &mut StdRng, input: u64) -> Self {
        let im = ImMapImmutEntryRun::new_worst_case(host, rng, input);
        let second_map_ref = im.map.clone();
        Self { im, second_map_ref }
    }

    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> Self {
        let im = ImMapImmutEntryRun::new_random_case(host, rng, input);
        let second_map_ref = im.map.clone();
        Self { im, second_map_ref }
    }

    fn run(&mut self, iter: u64, _host: &Host) {
        let _ = self
            .im
            .map
            .get_mut(&self.im.keys[iter as usize % self.im.keys.len()]);
    }

    fn get_input(&self, host: &Host) -> u64 {
        self.im.get_input(host)
    }
}
