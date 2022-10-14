use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, Rng};
use soroban_env_host::{budget::CostType, EnvVal, Host, RawVal};

pub(crate) struct ImMapMutEntryRun {
    map: im_rc::OrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>>,
    probes: Vec<EnvVal<Host, RawVal>>,
}

fn make_env_val(host: &Host, i: u32) -> EnvVal<Host, RawVal> {
    EnvVal {
        env: host.clone(),
        val: RawVal::from_u32(i),
    }
}

impl HostCostMeasurement for ImMapMutEntryRun {
    const COST_TYPE: CostType = CostType::ImMapMutEntry;
    const RUN_ITERATIONS: u64 = 1000;

    fn new_best_case(host: &Host, _rng: &mut StdRng) -> Self {
        let map: im_rc::OrdMap<_, _> = [{
            let ev = make_env_val(host, 1);
            (ev.clone(), ev)
        }]
        .into_iter()
        .collect();
        let probes = map.keys().cloned().collect();
        Self { map, probes }
    }

    fn new_worst_case(host: &Host, _rng: &mut StdRng, input: u64) -> Self {
        let input = input * 10000;
        let map: im_rc::OrdMap<_, _> = (0..input as u32)
            .map(|k| {
                let ev = make_env_val(host, k);
                (ev.clone(), ev)
            })
            .collect();
        let probes = [0, u32::MAX]
            .into_iter()
            .map(|k| make_env_val(host, k))
            .collect();
        Self { map, probes }
    }

    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> Self {
        let map: im_rc::OrdMap<_, _> = (0..input)
            .map(|_| {
                let ev = make_env_val(host, rng.gen::<u32>());
                (ev.clone(), ev)
            })
            .collect();
        let probes = (0..map.len())
            .into_iter()
            .map(|_| make_env_val(host, rng.gen::<u32>()))
            .collect();
        Self { map, probes }
    }

    fn run(&mut self, iter: u64, _host: &Host) {
        let key = &self.probes[iter as usize % self.probes.len()];
        let _ = self.map.get_mut(key);
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.map.len() as u64
    }
}
