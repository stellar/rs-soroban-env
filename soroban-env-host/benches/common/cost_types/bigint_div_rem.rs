use crate::common::HostCostMeasurement;
use num_bigint::{BigInt, Sign};
use rand::{rngs::StdRng, Rng};
use soroban_env_host::{cost_runner::BigIntDivRemRun, Host};

pub(crate) struct BigIntDivRemMeasure;

// This measures the costs of doing div_rem on the bigint type. The input value
// is the number of bits. The underlying code is variable-time in the bit size,
// but we expect that by bounding the bit size we can make a reasonably tight
// constant or linear-function upper bound. Examine the table and histogram.
impl HostCostMeasurement for BigIntDivRemMeasure {
    type Runner = BigIntDivRemRun;

    fn new_best_case(_host: &Host, _rng: &mut StdRng) -> (BigInt, BigInt) {
        let a: BigInt = 1u64.into();
        let b: BigInt = 1u64.into();
        (a, b)
    }

    fn new_worst_case(_host: &Host, _rng: &mut StdRng, input: u64) -> (BigInt, BigInt) {
        let input = input * 256;
        fn repeating_byte_bigint(byte: u8, input: u64) -> BigInt {
            let buf: Vec<u8> = (0..input / 8).map(|_| byte).collect();
            let a = BigInt::from_bytes_be(Sign::Plus, &buf);
            let one: BigInt = 1.into();
            a + one
        }
        // TODO: unclear if this really represents "worst case", it's
        // just a guess.
        let a: BigInt = repeating_byte_bigint(0xf7, input);
        let b: BigInt = repeating_byte_bigint(0xf6, 1 + (input / 2));
        (a, b)
    }

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> (BigInt, BigInt) {
        let input = input * 256;
        fn random_bigint(rng: &mut StdRng, input: u64) -> BigInt {
            let buf: Vec<u8> = (0..input / 8).map(|_| rng.gen::<u8>()).collect();
            let a = BigInt::from_bytes_be(Sign::Plus, &buf);
            let one: BigInt = 1.into();
            a + one
        }
        let a: BigInt = random_bigint(rng, input);
        let b: BigInt = random_bigint(rng, input);
        (a, b)
    }
}
