use crate::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{cost_runner::*, Host};

fn randvec(rng: &mut StdRng, len: u64) -> Vec<u8> {
    let mut res: Vec<u8> = vec![0; len as usize];
    rng.fill_bytes(res.as_mut_slice());
    res
}

pub struct MemCmpMeasure;
impl HostCostMeasurement for MemCmpMeasure {
    type Runner = MemCmpRun;

    const STEP_SIZE: u64 = 4096;

    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        let len = 1 + input * Self::STEP_SIZE;
        let a = randvec(rng, len);
        let b = randvec(rng, len);
        (a, b)
    }

    fn new_worst_case(
        _host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        let len = 1 + input * Self::STEP_SIZE;
        let a = randvec(rng, len);
        (a.clone(), a)
    }
}
