use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{budget::CostType, EnvVal, Host, RawVal};

pub(crate) struct ImMapImmutEntryRun {
    map: im_rc::OrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>>,
    input: u64,
}

/// Measures the costs of accessing maps of varying sizes.
impl HostCostMeasurement for ImMapImmutEntryRun {
    const COST_TYPE: CostType = CostType::ImMapImmutEntry;

    fn new_random_case(host: &Host, _rng: &mut StdRng, input: u64) -> Self {
        let size = input * 100;
        let map: im_rc::OrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>> = (0..size)
            .map(|k| {
                let ev = EnvVal {
                    env: host.clone(),
                    val: RawVal::from_u32(k as u32),
                };
                (ev.clone(), ev)
            })
            .collect();
        Self { map, input }
    }

    fn run(&mut self, _iter: u64, host: &Host) {
        let ev = EnvVal {
            env: host.clone(),
            val: RawVal::from_u32((self.input / 2) as u32),
        };
        let _ = self.map.get(&ev);
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.input
    }
}
