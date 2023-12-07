use crate::{common::util::randvec, HostCostMeasurement};
use rand::rngs::StdRng;
use soroban_env_host::{cost_runner::*, Host};

pub struct MemCmpMeasure;
impl HostCostMeasurement for MemCmpMeasure {
    type Runner = MemCmpRun;

    const STEP_SIZE: u64 = 4096;

    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        let len = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        let a = randvec(rng, len);
        let b = randvec(rng, len);
        (a, b)
    }

    fn new_worst_case(
        _host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        let len = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        let a = randvec(rng, len);
        (a.clone(), a)
    }
}
