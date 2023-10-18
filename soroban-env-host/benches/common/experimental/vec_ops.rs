use crate::common::{util, HostCostMeasurement};
use rand::{rngs::StdRng, seq::SliceRandom};
use soroban_env_host::{
    cost_runner::{VecEntryRun, VecEntrySample},
    Host, MeteredVector,
};

pub(crate) struct VecEntryMeasure;
// Measures the costs of accessing vectors of various sizes. It should have zero
// memory cost and logarithmic cpu cost, which we will approximate with an upper
// bound on vector size.
impl HostCostMeasurement for VecEntryMeasure {
    type Runner = VecEntryRun;

    // Random case is worst case.
    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> VecEntrySample {
        let input = 1 + input * Self::STEP_SIZE;
        let ov = util::u32_iter_to_val_iter(0..(input as u32)).collect();
        let vec: MeteredVector<_> = MeteredVector::from_vec(ov).unwrap();
        let mut idxs: Vec<usize> = (0..input as usize).collect();
        idxs.shuffle(rng);
        VecEntrySample { vec, idxs }
    }
}
