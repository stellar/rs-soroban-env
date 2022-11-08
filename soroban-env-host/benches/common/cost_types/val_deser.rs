use soroban_env_host::{cost_runner::ValDeserRun, xdr::WriteXdr};

use super::ValSerMeasure;
use crate::common::HostCostMeasurement;

pub(crate) struct ValDeserMeasure;

impl HostCostMeasurement for ValDeserMeasure {
    type Runner = ValDeserRun;

    fn new_random_case(
        host: &soroban_env_host::Host,
        rng: &mut rand::prelude::StdRng,
        input: u64,
    ) -> Vec<u8> {
        let scval = ValSerMeasure::new_random_case(host, rng, input);
        scval.to_xdr().unwrap()
    }
}
