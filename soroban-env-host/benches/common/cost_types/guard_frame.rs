use crate::common::HostCostMeasurement;
use soroban_env_host::{
    cost_runner::GuardFrameRun,
    xdr::{ContractExecutable, Hash, ScContractInstance},
    Symbol,
};

pub(crate) struct GuardFrameMeasure;

impl HostCostMeasurement for GuardFrameMeasure {
    type Runner = GuardFrameRun;

    fn new_best_case(
        _host: &soroban_env_host::Host,
        _rng: &mut rand::prelude::StdRng,
    ) -> (Hash, Symbol, ScContractInstance) {
        let id: Hash = [0; 32].into();
        let fun: Symbol = Symbol::try_from_small_str("add").unwrap();
        let empty_instance = ScContractInstance {
            executable: ContractExecutable::Token,
            storage: None,
        };
        (id, fun, empty_instance)
    }

    fn new_random_case(
        host: &soroban_env_host::Host,
        rng: &mut rand::prelude::StdRng,
        _input: u64,
    ) -> (Hash, Symbol, ScContractInstance) {
        Self::new_best_case(host, rng)
    }
}
