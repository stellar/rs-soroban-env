use crate::common::{util, HostCostMeasurement};
use rand::{rngs::StdRng, seq::SliceRandom};
use soroban_env_host::{
    budget::AsBudget,
    cost_runner::{VecEntryRun, VecEntrySample, VecNewRun},
    Host, MeteredVector, RawVal,
};

pub(crate) struct VecNewMeasure;
/// Measures the costs of allocating 0-sized vectors, without injecting to the host storage.
impl HostCostMeasurement for VecNewMeasure {
    type Runner = VecNewRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Vec<RawVal> {
        let input = 1 + input * Self::STEP_SIZE;
        let mut ov: Vec<RawVal> = util::to_rawval_u32(0..(input as u32)).collect();
        ov.shuffle(rng);
        ov
    }
}

pub(crate) struct VecEntryMeasure;
// Measures the costs of accessing vectors of various sizes. It should have zero
// memory cost and logarithmic cpu cost, which we will approximate with an upper
// bound on vector size.
impl HostCostMeasurement for VecEntryMeasure {
    type Runner = VecEntryRun;

    const STEP_SIZE: u64 = 100;

    // Random case is worst case.
    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> VecEntrySample {
        let input = 1 + input * Self::STEP_SIZE;
        let ov = util::to_rawval_u32(0..(input as u32)).collect();
        let vec: MeteredVector<_> = MeteredVector::from_vec(ov, host.as_budget()).unwrap();
        let mut idxs: Vec<usize> = (0..input as usize).collect();
        idxs.shuffle(rng);
        VecEntrySample { vec, idxs }
    }
}
