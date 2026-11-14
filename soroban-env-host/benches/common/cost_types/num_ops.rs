use crate::common::{util, HostCostMeasurement};
use rand::{rngs::StdRng, RngCore};
use soroban_env_common::{Env, EnvBase};
use soroban_env_host::{
    cost_runner::{Int256AddSubRun, Int256DivRun, Int256MulRun, Int256PowRun, Int256ShiftRun},
    Host, I256Val, U32Val, I256,
};

// These are best guesses.
fn worst_case_add_pair() -> (I256, I256) {
    (I256::MAX / 2, I256::MAX / 2 - 1)
}
fn worst_case_mul_pair() -> (I256, I256) {
    // Both |operands| >= 2, so I256 `checked_mul` runs its 256-bit division
    // overflow check. A divisor of 129 bits or more takes the Knuth long
    // division path, which is the most expensive one; opposite signs add a
    // negation. The product must not overflow, or the cheaper error path runs
    // instead.
    (
        (I256::new(1) << 124u32) + 1,
        -((I256::new(1) << 128u32) + 1),
    )
}
fn worst_case_div_pair() -> (I256, I256) {
    let a: I256 = util::repeating_byte_i256(0xf7, 32);
    let b: I256 = util::repeating_byte_i256(0xf6, 17);
    (a, b)
}
fn worst_case_pow_pair() -> (I256, u32) {
    // `checked_pow` is square-and-multiply, so an all-ones exponent runs the
    // most `checked_mul` calls: 63 for a base that never overflows (0 or +-1).
    // Each of those skips the division overflow check (|x| < 2) but still does
    // the full 256-bit multiplication, and 63 of them cost more than the 15
    // multiplications plus 14 divisions that base 2 runs before overflowing.
    (I256::new(1), u32::MAX)
}
fn worst_case_shift_pair() -> (I256, u32) {
    // The lhs must be an object (not representable as a small value) so that
    // the object visit is included, and the result must be small so that no
    // separately-charged allocation offsets it.
    (I256::MAX, 1)
}

macro_rules! impl_int256_measure {
    ($measure: ident, $runner: ident, $worst: ident) => {
        pub(crate) struct $measure;

        impl HostCostMeasurement for $measure {
            type Runner = $runner;

            fn new_worst_case(host: &Host, _rng: &mut StdRng, _input: u64) -> (I256Val, I256Val) {
                let (lhs, rhs) = $worst();
                let bytes = host
                    .bytes_new_from_slice(lhs.to_be_bytes().as_slice())
                    .unwrap();
                let lhs_val = host.i256_val_from_be_bytes(bytes).unwrap();
                let bytes = host
                    .bytes_new_from_slice(rhs.to_be_bytes().as_slice())
                    .unwrap();
                let rhs_val = host.i256_val_from_be_bytes(bytes).unwrap();
                (lhs_val, rhs_val)
            }

            fn new_random_case(host: &Host, rng: &mut StdRng, _input: u64) -> (I256Val, I256Val) {
                let mut bytes = [0; 32];
                rng.fill_bytes(bytes.as_mut_slice());
                let bo = host.bytes_new_from_slice(bytes.as_slice()).unwrap();
                let lhs_val = host.i256_val_from_be_bytes(bo).unwrap();
                rng.fill_bytes(bytes.as_mut_slice());
                let bo = host.bytes_new_from_slice(bytes.as_slice()).unwrap();
                let rhs_val = host.i256_val_from_be_bytes(bo).unwrap();
                (lhs_val, rhs_val)
            }
        }
    };
}
impl_int256_measure!(Int256AddSubMeasure, Int256AddSubRun, worst_case_add_pair);
impl_int256_measure!(Int256MulMeasure, Int256MulRun, worst_case_mul_pair);
impl_int256_measure!(Int256DivMeasure, Int256DivRun, worst_case_div_pair);

macro_rules! impl_int256_measure_rhs_u32 {
    ($measure: ident, $runner: ident, $worst: ident) => {
        pub(crate) struct $measure;

        impl HostCostMeasurement for $measure {
            type Runner = $runner;

            fn new_worst_case(host: &Host, _rng: &mut StdRng, _input: u64) -> (I256Val, U32Val) {
                let (lhs, rhs) = $worst();
                let bytes = host
                    .bytes_new_from_slice(lhs.to_be_bytes().as_slice())
                    .unwrap();
                let lhs_val = host.i256_val_from_be_bytes(bytes).unwrap();
                (lhs_val, U32Val::from(rhs))
            }

            fn new_random_case(host: &Host, rng: &mut StdRng, _input: u64) -> (I256Val, U32Val) {
                let mut bytes = [0; 32];
                rng.fill_bytes(bytes.as_mut_slice());
                let bo = host.bytes_new_from_slice(bytes.as_slice()).unwrap();
                let lhs_val = host.i256_val_from_be_bytes(bo).unwrap();
                (lhs_val, U32Val::from(rng.next_u32()))
            }
        }
    };
}
impl_int256_measure_rhs_u32!(Int256PowMeasure, Int256PowRun, worst_case_pow_pair);
impl_int256_measure_rhs_u32!(Int256ShiftMeasure, Int256ShiftRun, worst_case_shift_pair);
