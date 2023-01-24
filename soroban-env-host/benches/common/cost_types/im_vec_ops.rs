use crate::common::{util, HostCostMeasurement};
use rand::{rngs::StdRng, seq::SliceRandom};
use soroban_env_host::{
    cost_runner::{ImVecEntryRun, ImVecEntrySample, ImVecNewRun},
    Host, MeteredVector, RawVal,
};

pub(crate) struct ImVecNewMeasure;
/// Measures the costs of allocating 0-sized vectors, without injecting to the host storage.
impl HostCostMeasurement for ImVecNewMeasure {
    type Runner = ImVecNewRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Vec<RawVal> {
        let input = input * 1000;
        let mut ov: Vec<RawVal> = util::to_rawval_u32(0..(input as u32)).collect();
        ov.shuffle(rng);
        ov
    }
}

pub(crate) struct ImVecEntryMeasure;
// Measures the costs of accessing vectors of various sizes. It should have zero
// memory cost and logarithmic cpu cost, which we will approximate with an upper
// bound on vector size.
impl HostCostMeasurement for ImVecEntryMeasure {
    type Runner = ImVecEntryRun;

    // Random case is worst case.
    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> ImVecEntrySample {
        let input = input * 100;
        let ov = util::to_rawval_u32(0..(input as u32)).collect();
        let vec: MeteredVector<_> = MeteredVector::from_vec(ov).unwrap();
        let mut idxs: Vec<usize> = (0..input as usize).collect();
        idxs.shuffle(rng);
        ImVecEntrySample { vec, idxs }
    }
}
