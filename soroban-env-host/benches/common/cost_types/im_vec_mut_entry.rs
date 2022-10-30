use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::{ImVecMutEntryRun, ImVecMutEntrySample},
    Host,
};

use super::im_vec_immut_entry::ImVecImmutEntryMeasure;

pub(crate) struct ImVecMutEntryMeasure;

// This is just a variant of ImVecImmutEntryRun that calls the get_mut method on
// a multiply-referenced vec, causing a copy-on-write of some nodes. It should
// cost a nearly-constant (perhaps very-slow-log) amount of memory and CPU.
impl HostCostMeasurement for ImVecMutEntryMeasure {
    type Runner = ImVecMutEntryRun;

    fn new_best_case(host: &Host, rng: &mut StdRng) -> ImVecMutEntrySample {
        let im = ImVecImmutEntryMeasure::new_best_case(host, rng);
        let second_vec_ref = im.vec.clone();
        ImVecMutEntrySample { im, second_vec_ref }
    }

    // Random case is worst case.
    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> ImVecMutEntrySample {
        let im = ImVecImmutEntryMeasure::new_random_case(host, rng, input);
        let second_vec_ref = im.vec.clone();
        ImVecMutEntrySample { im, second_vec_ref }
    }
}
