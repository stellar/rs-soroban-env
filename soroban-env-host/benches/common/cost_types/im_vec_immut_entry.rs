use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{budget::CostType, EnvVal, Host, RawVal};

pub(crate) struct ImVecImmutEntryRun {
    vec: im_rc::Vector<EnvVal<Host, RawVal>>,
}

impl HostCostMeasurement for ImVecImmutEntryRun {
    const COST_TYPE: CostType = CostType::ImVecImmutEntry;
    const RUN_ITERATIONS: u64 = 100;

    fn new_random_case(host: &Host, _rng: &mut StdRng, input: u64) -> Self {
        let size = 1 + (input * 100);
        let vec: im_rc::Vector<EnvVal<Host, RawVal>> = (0..size)
            .map(|k| EnvVal {
                env: host.clone(),
                val: RawVal::from_u32(k as u32),
            })
            .collect();
        Self { vec }
    }

    fn run(&mut self, iter: u64, _host: &Host) {
        let _ = self.vec.get(iter as usize % self.vec.len());
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.vec.len() as u64
    }
}
