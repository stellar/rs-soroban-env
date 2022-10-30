use crate::common::{util, HostCostMeasurement};
use rand::{rngs::StdRng, seq::SliceRandom};
use soroban_env_host::{
    cost_runner::{ImVecImmutEntryRun, ImVecImmutEntrySample},
    Host,
};

pub(crate) struct ImVecImmutEntryMeasure;

// Measures the costs of accessing vectors of various sizes. It should have zero
// memory cost and logarithmic cpu cost, which we will approximate with an upper
// bound on vector size.
impl HostCostMeasurement for ImVecImmutEntryMeasure {
    type Runner = ImVecImmutEntryRun;

    fn new_best_case(host: &Host, _rng: &mut StdRng) -> ImVecImmutEntrySample {
        let vec = util::to_envval_u32(host, 0..1).collect();
        let idxs = [0].to_vec();
        ImVecImmutEntrySample { vec, idxs }
    }

    // Random case is worst case.
    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> ImVecImmutEntrySample {
        let input = 1 + (input * 10000);
        let vec = util::to_envval_u32(host, 0..(input as u32)).collect();
        let mut idxs: Vec<usize> = (0..input as usize).collect();
        idxs.shuffle(rng);
        ImVecImmutEntrySample { vec, idxs }
    }
}
