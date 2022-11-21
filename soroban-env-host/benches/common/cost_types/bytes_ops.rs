use crate::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{cost_runner::*, Host};
pub struct BytesCloneMeasure;
impl HostCostMeasurement for BytesCloneMeasure {
    type Runner = BytesCloneRun;

    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        let input = 1 + input * 100;
        let mut res: Vec<u8> = vec![0; input as usize];
        rng.fill_bytes(res.as_mut_slice());
        res
    }
}
pub struct BytesDelMeasure;
impl HostCostMeasurement for BytesDelMeasure {
    type Runner = BytesDelRun;

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        let v = BytesCloneMeasure::new_random_case(host, rng, input);
        let i = rng.next_u32() as usize % v.len();
        (v, i)
    }

    fn new_worst_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        let v = BytesCloneMeasure::new_random_case(host, rng, input);
        (v, 0)
    }
}
pub struct BytesPushMeasure;
impl HostCostMeasurement for BytesPushMeasure {
    type Runner = BytesPushRun;

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        BytesCloneMeasure::new_random_case(host, rng, input)
    }
}
pub struct BytesPopMeasure;
impl HostCostMeasurement for BytesPopMeasure {
    type Runner = BytesPopRun;

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        BytesCloneMeasure::new_random_case(host, rng, input)
    }
}
pub struct BytesInsertMeasure;
impl HostCostMeasurement for BytesInsertMeasure {
    type Runner = BytesInsertRun;

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        BytesDelMeasure::new_random_case(host, rng, input)
    }

    fn new_worst_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        BytesDelMeasure::new_worst_case(host, rng, input)
    }
}
pub struct BytesAppendMeasure;
impl HostCostMeasurement for BytesAppendMeasure {
    type Runner = BytesAppendRun;

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        let a = BytesCloneMeasure::new_random_case(host, rng, input);
        let b = BytesCloneMeasure::new_random_case(host, rng, input);
        (a, b)
    }
}
pub struct BytesCmpMeasure;
impl HostCostMeasurement for BytesCmpMeasure {
    type Runner = BytesCmpRun;

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        BytesAppendMeasure::new_random_case(host, rng, input)
    }

    fn new_worst_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        let a = BytesCloneMeasure::new_random_case(host, rng, input);
        (a.clone(), a)
    }
}
