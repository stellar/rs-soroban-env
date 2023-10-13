use crate::common::HostCostMeasurement;
use curve25519_dalek::{constants, edwards, scalar};
use rand::{rngs::StdRng, Rng, RngCore};
use soroban_env_host::{
    cost_runner::{Ed25519ScalarMulRun, Ed25519ScalarMulSample},
    Host,
};

// This measures the costs of doing an Ed25519 scalar multiply, which is a
// component of signature verification and is the only part advertised as
// variable time. The input value is ignored. The underlying operation is
// nonlinear and the point of this measurement is to investigate how wide the
// variation is in the histogram.
#[allow(non_snake_case)]
pub(crate) struct Ed25519ScalarMulMeasure {
    a: scalar::Scalar,
    A: edwards::EdwardsPoint,
    b: scalar::Scalar,
}
#[allow(non_snake_case)]
impl HostCostMeasurement for Ed25519ScalarMulMeasure {
    type Runner = Ed25519ScalarMulRun;

    fn new_best_case(_host: &Host, _rng: &mut StdRng) -> Ed25519ScalarMulSample {
        let a = scalar::Scalar::ONE;
        let b = scalar::Scalar::ONE;
        let A = constants::ED25519_BASEPOINT_COMPRESSED
            .decompress()
            .unwrap();
        Ed25519ScalarMulSample { a, b, A }
    }

    fn new_worst_case(_host: &Host, _rng: &mut StdRng, _input: u64) -> Ed25519ScalarMulSample {
        let a = scalar::Scalar::from_bytes_mod_order([0xff; 32]);
        let b = a.clone();
        let A = constants::ED25519_BASEPOINT_COMPRESSED
            .decompress()
            .unwrap();
        Ed25519ScalarMulSample { a, b, A }
    }

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Ed25519ScalarMulSample {
        fn random_scalar(rng: &mut StdRng) -> scalar::Scalar {
            let mut buf: [u8; 32] = [0; 32];
            rng.fill_bytes(&mut buf);
            // Here we zero-out some number of bits from the top, making the scalar
            // smaller and thus the work less. It's not really plausible for natural
            // occurrences here to have more than a handful of contiguous zeroes here
            // but for thoroughness we'll let it go up to 64 (8 bytes).
            for i in 0..rng.gen_range(0..9) {
                buf[31 - i] = 0
            }
            scalar::Scalar::from_bytes_mod_order(buf)
        }
        let a = random_scalar(rng);
        let b = random_scalar(rng);
        let A = constants::ED25519_BASEPOINT_COMPRESSED
            .decompress()
            .unwrap();
        Ed25519ScalarMulSample { a, b, A }
    }
}
