use crate::common::HostCostMeasurement;
use curve25519_dalek::{constants, edwards, scalar};
use rand::{rngs::StdRng, Rng, RngCore};
use soroban_env_host::{budget::CostType, Host};

#[allow(non_snake_case)]
pub(crate) struct EdwardsPointCurve25519ScalarMulRun {
    a: scalar::Scalar,
    A: edwards::EdwardsPoint,
    b: scalar::Scalar,
}
#[allow(non_snake_case)]
impl HostCostMeasurement for EdwardsPointCurve25519ScalarMulRun {
    const COST_TYPE: CostType = CostType::EdwardsPointCurve25519ScalarMul;
    const RUN_ITERATIONS: u64 = 100;

    fn run(&mut self, _iter: u64, _host: &Host) {
        edwards::EdwardsPoint::vartime_double_scalar_mul_basepoint(&self.a, &self.A, &self.b);
    }

    fn new_best_case(_host: &Host, _rng: &mut StdRng) -> Self {
        let a = scalar::Scalar::one();
        let b = scalar::Scalar::one();
        let A = constants::ED25519_BASEPOINT_COMPRESSED
            .decompress()
            .unwrap();
        Self { a, b, A }
    }

    fn new_worst_case(_host: &Host, _rng: &mut StdRng, _input: u64) -> Self {
        let a = scalar::Scalar::from_bytes_mod_order([0xff; 32]);
        let b = a.clone();
        let A = constants::ED25519_BASEPOINT_COMPRESSED
            .decompress()
            .unwrap();
        Self { a, b, A }
    }

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Self {
        fn random_scalar(rng: &mut StdRng) -> scalar::Scalar {
            let mut buf: [u8; 32] = [0; 32];
            rng.fill_bytes(&mut buf);
            // Here we zero-out some number of bits from the top, making the scalar
            // smaller and thus the work less. It's not really plausible for natural
            // occurrences here to have more than a handful of contiguous zeroes here
            // but for thoroughness we'll let it go up to 64 (8 bytes).
            for i in 0..rng.gen_range(0, 9) {
                buf[31 - i] = 0
            }
            scalar::Scalar::from_bytes_mod_order(buf)
        }
        let a = random_scalar(rng);
        let b = random_scalar(rng);
        let A = constants::ED25519_BASEPOINT_COMPRESSED
            .decompress()
            .unwrap();
        Self { a, b, A }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        1
    }
}
