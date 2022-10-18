use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, seq::SliceRandom};
use soroban_env_host::{budget::CostType, EnvVal, Host, RawVal};

use super::util;

pub(crate) struct ImVecImmutEntryRun {
    pub(crate) vec: im_rc::Vector<EnvVal<Host, RawVal>>,
    pub(crate) idxs: Vec<usize>,
}

// Measures the costs of accessing vectors of various sizes. It should have zero
// memory cost and logarithmic cpu cost, which we will approximate with an upper
// bound on vector size.
impl HostCostMeasurement for ImVecImmutEntryRun {
    const COST_TYPE: CostType = CostType::ImVecImmutEntry;
    const RUN_ITERATIONS: u64 = 100;

    fn new_best_case(host: &Host, _rng: &mut StdRng) -> Self {
        let vec = util::to_envval_u32(host, 0..1).collect();
        let idxs = [0].to_vec();
        Self { vec, idxs }
    }

    // Random case is worst case.
    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> Self {
        let input = 1 + (input * 10000);
        let vec = util::to_envval_u32(host, 0..(input as u32)).collect();
        let mut idxs: Vec<usize> = (0..input as usize).collect();
        idxs.shuffle(rng);
        Self { vec, idxs }
    }

    fn run(&mut self, iter: u64, _host: &Host) {
        let _ = self.vec.get(self.idxs[iter as usize % self.idxs.len()]);
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.vec.len() as u64
    }
}
