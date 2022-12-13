use crate::common::{util, HostCostMeasurement};
use rand::{rngs::StdRng, seq::SliceRandom};
use soroban_env_host::{
    cost_runner::{ImVecImmutEntryRun, ImVecImmutEntrySample, ImVecNewRun},
    Host, MeteredVector,
};

pub(crate) struct ImVecNewMeasure;
/// Measures the costs of allocating 0-sized vectors, without injecting to the host storage.
impl HostCostMeasurement for ImVecNewMeasure {
    type Runner = ImVecNewRun;

    fn new_random_case(_host: &Host, _rng: &mut StdRng, _input: u64) {
        ()
    }
}

pub(crate) struct ImVecImmutEntryMeasure;
// Measures the costs of accessing vectors of various sizes. It should have zero
// memory cost and logarithmic cpu cost, which we will approximate with an upper
// bound on vector size.
impl HostCostMeasurement for ImVecImmutEntryMeasure {
    type Runner = ImVecImmutEntryRun;

    fn new_best_case(_host: &Host, _rng: &mut StdRng) -> ImVecImmutEntrySample {
        let ov = util::to_rawval_u32(0..1).collect();
        let vec: MeteredVector<_> = MeteredVector::from_vec(ov).unwrap();
        let idxs = [0].to_vec();
        ImVecImmutEntrySample { vec, idxs }
    }

    // Random case is worst case.
    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> ImVecImmutEntrySample {
        let input = 1 + (input * 100);
        let ov = util::to_rawval_u32(0..(input as u32)).collect();
        let vec: MeteredVector<_> = MeteredVector::from_vec(ov).unwrap();
        let mut idxs: Vec<usize> = (0..input as usize).collect();
        idxs.shuffle(rng);
        ImVecImmutEntrySample { vec, idxs }
    }
}
