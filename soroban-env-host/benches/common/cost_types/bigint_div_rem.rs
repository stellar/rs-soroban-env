use crate::common::HostCostMeasurement;
use num_bigint::{BigInt, Sign};
use num_integer::Integer;
use rand::{rngs::StdRng, Rng};
use soroban_env_host::{budget::CostType, Host};

pub(crate) struct BigIntDivRemRun {
    a: BigInt,
    b: BigInt,
    input: u64,
}

impl HostCostMeasurement for BigIntDivRemRun {
    const COST_TYPE: CostType = CostType::BigIntDivRem;
    const RUN_ITERATIONS: u64 = 100;

    fn new_best_case(_host: &Host, _rng: &mut StdRng) -> Self {
        let a: BigInt = 1u64.into();
        let b: BigInt = 1u64.into();
        Self { a, b, input: 1 }
    }

    fn new_worst_case(_host: &Host, _rng: &mut StdRng, input: u64) -> Self {
        let input = input * 256;
        fn repeating_byte_bigint(byte: u8, input: u64) -> BigInt {
            let buf: Vec<u8> = (0..input / 8).map(|_| byte).collect();
            let a = BigInt::from_bytes_be(Sign::Plus, &buf);
            let one: BigInt = 1.into();
            a + one
        }
        let a: BigInt = repeating_byte_bigint(0xf7, input);
        let b: BigInt = repeating_byte_bigint(0xf6, 1 + (input / 2));
        Self { a, b, input }
    }

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Self {
        let input = input * 256;
        fn random_bigint(rng: &mut StdRng, input: u64) -> BigInt {
            let buf: Vec<u8> = (0..input / 8).map(|_| rng.gen::<u8>()).collect();
            let a = BigInt::from_bytes_be(Sign::Plus, &buf);
            let one: BigInt = 1.into();
            a + one
        }
        let a: BigInt = random_bigint(rng, input);
        let b: BigInt = random_bigint(rng, input);
        Self { a, b, input }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.input
    }

    fn run(&mut self, _iter: u64, _host: &Host) {
        self.a.div_rem(&self.b);
    }
}
