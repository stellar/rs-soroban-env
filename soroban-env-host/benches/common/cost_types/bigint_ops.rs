use crate::common::{util, HostCostMeasurement};
use num_bigint::BigInt;
use rand::{rngs::StdRng, Rng, RngCore};
use soroban_env_host::{cost_runner::*, Host, MeteredBigInt};

pub(crate) struct BigIntAddSubMeasure;
impl HostCostMeasurement for BigIntAddSubMeasure {
    type Runner = BigIntAddSubRun;

    fn new_best_case(host: &Host, _rng: &mut StdRng) -> (MeteredBigInt, MeteredBigInt) {
        let a = MeteredBigInt::from_u64(host.budget_cloned(), 1).unwrap();
        let b = MeteredBigInt::from_u64(host.budget_cloned(), 1).unwrap();
        (a, b)
    }

    fn new_worst_case(
        host: &Host,
        _rng: &mut StdRng,
        input: u64,
    ) -> (MeteredBigInt, MeteredBigInt) {
        let input = input * 256;
        // TODO: unclear if this really represents "worst case", it's
        // just a guess.
        let a: BigInt = util::repeating_byte_bigint(0xf7, input);
        let b: BigInt = util::repeating_byte_bigint(0xf7, input);
        (
            MeteredBigInt::from_bigint(host.budget_cloned(), a),
            MeteredBigInt::from_bigint(host.budget_cloned(), b),
        )
    }

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> (MeteredBigInt, MeteredBigInt) {
        let input = input * 256;
        let a: BigInt = util::random_bigint(rng, input);
        let b: BigInt = util::random_bigint(rng, input);
        (
            MeteredBigInt::from_bigint(host.budget_cloned(), a),
            MeteredBigInt::from_bigint(host.budget_cloned(), b),
        )
    }
}

pub(crate) struct BigIntMulMeasure;
impl HostCostMeasurement for BigIntMulMeasure {
    type Runner = BigIntMulRun;

    fn new_best_case(host: &Host, rng: &mut StdRng) -> (MeteredBigInt, MeteredBigInt) {
        BigIntAddSubMeasure::new_best_case(host, rng)
    }

    fn new_worst_case(host: &Host, rng: &mut StdRng, input: u64) -> (MeteredBigInt, MeteredBigInt) {
        BigIntAddSubMeasure::new_worst_case(host, rng, input)
    }

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> (MeteredBigInt, MeteredBigInt) {
        BigIntAddSubMeasure::new_random_case(host, rng, input)
    }
}

// This measures the costs of doing div_rem on the bigint type. The input value
// is the number of bits. The underlying code is variable-time in the bit size,
// but we expect that by bounding the bit size we can make a reasonably tight
// constant or linear-function upper bound. Examine the table and histogram.
pub(crate) struct BigIntDivRemMeasure;
impl HostCostMeasurement for BigIntDivRemMeasure {
    type Runner = BigIntDivRemRun;

    fn new_best_case(host: &Host, rng: &mut StdRng) -> (MeteredBigInt, MeteredBigInt) {
        BigIntAddSubMeasure::new_best_case(host, rng)
    }

    fn new_worst_case(
        host: &Host,
        _rng: &mut StdRng,
        input: u64,
    ) -> (MeteredBigInt, MeteredBigInt) {
        let input = input * 256;
        // TODO: unclear if this really represents "worst case", it's
        // just a guess.
        let a: BigInt = util::repeating_byte_bigint(0xf7, input);
        let b: BigInt = util::repeating_byte_bigint(0xf6, 1 + (input / 2));
        (
            MeteredBigInt::from_bigint(host.budget_cloned(), a),
            MeteredBigInt::from_bigint(host.budget_cloned(), b),
        )
    }

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> (MeteredBigInt, MeteredBigInt) {
        BigIntAddSubMeasure::new_random_case(host, rng, input)
    }
}

pub(crate) struct BigIntBitwiseOpMeasure;
impl HostCostMeasurement for BigIntBitwiseOpMeasure {
    type Runner = BigIntBitwiseOpRun;

    fn new_best_case(host: &Host, rng: &mut StdRng) -> (MeteredBigInt, MeteredBigInt) {
        BigIntAddSubMeasure::new_best_case(host, rng)
    }

    fn new_worst_case(host: &Host, rng: &mut StdRng, input: u64) -> (MeteredBigInt, MeteredBigInt) {
        BigIntAddSubMeasure::new_worst_case(host, rng, input)
    }

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> (MeteredBigInt, MeteredBigInt) {
        BigIntAddSubMeasure::new_random_case(host, rng, input)
    }
}

pub(crate) struct BigIntShiftMeasure;
impl HostCostMeasurement for BigIntShiftMeasure {
    type Runner = BigIntShiftRun;

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> (MeteredBigInt, MeteredBigInt) {
        let a: BigInt = util::random_bigint(rng, input * 256);
        let b: BigInt = util::random_bigint(rng, input);
        (
            MeteredBigInt::from_bigint(host.budget_cloned(), a),
            MeteredBigInt::from_bigint(host.budget_cloned(), b),
        )
    }
}

pub(crate) struct BigIntCmpMeasure;
impl HostCostMeasurement for BigIntCmpMeasure {
    type Runner = BigIntCmpRun;

    fn new_best_case(host: &Host, rng: &mut StdRng) -> (MeteredBigInt, MeteredBigInt) {
        BigIntAddSubMeasure::new_best_case(host, rng)
    }

    fn new_worst_case(host: &Host, rng: &mut StdRng, input: u64) -> (MeteredBigInt, MeteredBigInt) {
        BigIntAddSubMeasure::new_worst_case(host, rng, input)
    }

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> (MeteredBigInt, MeteredBigInt) {
        BigIntAddSubMeasure::new_random_case(host, rng, input)
    }
}

pub(crate) struct BigIntGcdLcmMeasure;
impl HostCostMeasurement for BigIntGcdLcmMeasure {
    type Runner = BigIntGcdLcmRun;

    fn new_best_case(host: &Host, rng: &mut StdRng) -> (MeteredBigInt, MeteredBigInt) {
        BigIntDivRemMeasure::new_best_case(host, rng)
    }

    fn new_worst_case(host: &Host, rng: &mut StdRng, input: u64) -> (MeteredBigInt, MeteredBigInt) {
        BigIntDivRemMeasure::new_worst_case(host, rng, input)
    }

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> (MeteredBigInt, MeteredBigInt) {
        BigIntDivRemMeasure::new_random_case(host, rng, input)
    }
}

// TODO: this is not constant or linear
pub(crate) struct BigIntPowMeasure;
impl HostCostMeasurement for BigIntPowMeasure {
    type Runner = BigIntPowRun;

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> (MeteredBigInt, MeteredBigInt) {
        let a: BigInt = util::random_bigint(rng, input * 256);
        let pow = rng.next_u64() % 0xFF;
        (
            MeteredBigInt::from_bigint(host.budget_cloned(), a),
            MeteredBigInt::from_u64(host.budget_cloned(), pow).unwrap(),
        )
    }
}

// TODO: this is not constant or linear
pub(crate) struct BigIntPowModMeasure;
impl HostCostMeasurement for BigIntPowModMeasure {
    type Runner = BigIntPowModRun;

    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> (MeteredBigInt, MeteredBigInt, MeteredBigInt) {
        let a: BigInt = util::random_bigint(rng, input * 256);
        let pow = rng.next_u64() % 0xFF;
        let c: BigInt = util::random_bigint(rng, input * 256);
        (
            MeteredBigInt::from_bigint(host.budget_cloned(), a),
            MeteredBigInt::from_u64(host.budget_cloned(), pow).unwrap(),
            MeteredBigInt::from_bigint(host.budget_cloned(), c),
        )
    }
}

pub(crate) struct BigIntSqrtMeasure;
impl HostCostMeasurement for BigIntSqrtMeasure {
    type Runner = BigIntSqrtRun;

    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> MeteredBigInt {
        let a: BigInt = util::random_bigint(rng, input * 256);
        MeteredBigInt::from_bigint(host.budget_cloned(), a)
    }
}

pub(crate) struct BigIntFromBytesMeasure;
impl HostCostMeasurement for BigIntFromBytesMeasure {
    type Runner = BigIntFromBytesRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Vec<u8> {
        let input = input * 256;
        (0..input / 8).map(|_| rng.gen::<u8>()).collect()
    }
}

pub(crate) struct BigIntToBytesMeasure;
impl HostCostMeasurement for BigIntToBytesMeasure {
    type Runner = BigIntToBytesRun;

    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> MeteredBigInt {
        BigIntSqrtMeasure::new_random_case(host, rng, input)
    }
}

pub(crate) struct BigIntToRadixMeasure;
impl HostCostMeasurement for BigIntToRadixMeasure {
    type Runner = BigIntToRadixRun;

    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> (MeteredBigInt, u32) {
        let radix = (rng.next_u32() as u8) + 1;
        (
            BigIntSqrtMeasure::new_random_case(host, rng, input),
            radix as u32,
        )
    }
}
