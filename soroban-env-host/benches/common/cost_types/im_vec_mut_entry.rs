use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{budget::CostType, EnvVal, Host, RawVal};

use super::im_vec_immut_entry::ImVecImmutEntryRun;

pub(crate) struct ImVecMutEntryRun {
    im: ImVecImmutEntryRun,
    second_vec_ref: im_rc::Vector<EnvVal<Host, RawVal>>,
}

// This is just a variant of ImVecImmutEntryRun that calls the get_mut method on
// a multiply-referenced vec, causing a copy-on-write of some nodes. It should
// cost a nearly-constant (perhaps very-slow-log) amount of memory and CPU.
impl HostCostMeasurement for ImVecMutEntryRun {
    const COST_TYPE: CostType = CostType::ImVecMutEntry;
    const RUN_ITERATIONS: u64 = ImVecImmutEntryRun::RUN_ITERATIONS;

    fn new_best_case(host: &Host, rng: &mut StdRng) -> Self {
        let im = ImVecImmutEntryRun::new_best_case(host, rng);
        let second_vec_ref = im.vec.clone();
        Self { im, second_vec_ref }
    }

    // Random case is worst case.
    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> Self {
        let im = ImVecImmutEntryRun::new_random_case(host, rng, input);
        let second_vec_ref = im.vec.clone();
        Self { im, second_vec_ref }
    }

    fn run(&mut self, iter: u64, _host: &Host) {
        let _ = self
            .im
            .vec
            .get_mut(self.im.idxs[iter as usize % self.im.idxs.len()]);
    }

    fn get_input(&self, host: &Host) -> u64 {
        self.im.get_input(host)
    }
}
