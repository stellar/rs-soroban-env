use crate::common::HostCostMeasurement;
use soroban_env_host::{cost_runner::GuardFrameRun, xdr::Hash, Symbol};

pub(crate) struct GuardFrameMeasure;

impl HostCostMeasurement for GuardFrameMeasure {
    type Runner = GuardFrameRun;

    fn new_best_case(
        _host: &soroban_env_host::Host,
        _rng: &mut rand::prelude::StdRng,
    ) -> (Hash, Symbol) {
        let id: Hash = [0; 32].into();
        let fun: Symbol = Symbol::from_str("add");
        (id, fun)
    }

    fn new_random_case(
        host: &soroban_env_host::Host,
        rng: &mut rand::prelude::StdRng,
        _input: u64,
    ) -> (Hash, Symbol) {
        Self::new_best_case(host, rng)
    }
}
