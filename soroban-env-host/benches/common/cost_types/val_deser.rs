use soroban_env_host::{cost_runner::ValDeserRun, xdr::ScVal, xdr::WriteXdr};

use super::{ValSerMeasure, MAX_DEPTH};
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
        scval.0.to_xdr().unwrap()
    }

    // The worst case is a deeply nested structure, each level containing minimal
    // bytes to move around.
    fn new_worst_case(
        _host: &soroban_env_host::Host,
        _rng: &mut rand::prelude::StdRng,
        input: u64,
    ) -> Vec<u8> {
        let input = 1 + input * Self::STEP_SIZE;
        let elem_per_level = 1 + input / MAX_DEPTH;
        let mut v = ScVal::U64(0);
        let mut rem = input;
        for _i in 0..MAX_DEPTH {
            if rem == 0 {
                break;
            }
            let mut inner = vec![v; 1];
            inner.append(&mut vec![ScVal::U64(0); elem_per_level as usize]);
            v = ScVal::Vec(Some(inner.try_into().unwrap()));
            rem -= elem_per_level;
        }
        v.to_xdr().unwrap()
    }
}
