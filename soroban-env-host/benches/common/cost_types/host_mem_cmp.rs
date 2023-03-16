use crate::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{cost_runner::*, Host};

fn randvec(rng: &mut StdRng, input: u64) -> Vec<u8> {
    let input = 1 + input * 1000;
    let mut res: Vec<u8> = vec![0; input as usize];
    rng.fill_bytes(res.as_mut_slice());
    res
}

pub struct HostMemCmpMeasure;
impl HostCostMeasurement for HostMemCmpMeasure {
    type Runner = HostMemCmpRun;

    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        let a = randvec(rng, input);
        let b = randvec(rng, input);
        (a, b)
    }

    fn new_worst_case(
        _host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        let a = randvec(rng, input);
        (a.clone(), a)
    }
}
