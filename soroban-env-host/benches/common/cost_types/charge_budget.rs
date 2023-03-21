use crate::common::HostCostMeasurement;
use rand::Rng;
use soroban_env_host::{cost_runner::ChargeBudgetRun, Host};

pub(crate) struct ChargeBudgetMeasure;

// Measures the cost of invoking the budget charging machineary.
// Should have constant cpu cost and zero mem cost.
impl HostCostMeasurement for ChargeBudgetMeasure {
    type Runner = ChargeBudgetRun;

    fn new_random_case(_host: &Host, rng: &mut rand::prelude::StdRng, _input: u64) -> u64 {
        rng.gen_range(0, 10000)
    }
}
