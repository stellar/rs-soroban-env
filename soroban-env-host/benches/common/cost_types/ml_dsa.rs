use crate::common::HostCostMeasurement;
use ml_dsa::{
    common::typenum::Unsigned, MlDsa44, MlDsa65, MlDsa87, MlDsaParams, Signature, SigningKey,
};
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{
    cost_runner::{
        MlDsa44DecodeSignatureRun, MlDsa44DecodeVerifyingKeyRun, MlDsa65DecodeSignatureRun,
        MlDsa65DecodeVerifyingKeyRun, MlDsa87DecodeSignatureRun, MlDsa87DecodeVerifyingKeyRun,
        MlDsaDecodeSignatureSample, MlDsaDecodeVerifyingKeySample, MlDsaVerifySigSample,
        VerifyMlDsa44SigRun, VerifyMlDsa65SigRun, VerifyMlDsa87SigRun,
    },
    Host,
};

/// The context string is fixed at zero length for calibration.
///
/// The charged input is `msg.len() + ctx.len()`, and the SHAKE-256 absorption
/// that computes `mu` takes `tr || 0x00 || len(ctx) || ctx || M`. The absorbed
/// length is therefore `66 + (msg + ctx)`. context and message bytes are
/// interchangeable in the cost model. Fixing it at 0 matches the common case
/// (no domain separator).
const CTX_LEN: usize = 0;

/// SHAKE-256 rate in bytes: (1600 - 512) / 8.
///
/// The linear component of verification is the SHAKE-256 absorption computing
/// `mu`, which costs one Keccak-f[1600] permutation per 136-byte block.
const SHAKE256_RATE: u64 = 136;

fn random_signing_key<P: MlDsaParams>(rng: &mut StdRng) -> SigningKey<P> {
    let mut seed = [0u8; 32];
    rng.fill_bytes(&mut seed);
    SigningKey::<P>::from_seed(&seed.into())
}

fn sign_random<P: MlDsaParams>(rng: &mut StdRng) -> Vec<u8> {
    let sk = random_signing_key::<P>(rng);
    let mut msg = [0u8; 32];
    rng.fill_bytes(&mut msg);
    sk.expanded_key()
        .sign_deterministic(&msg, &[])
        .expect("deterministic signing")
        .encode()
        .to_vec()
}

/// A signature carrying the heaviest hint the encoding admits: `omega` set
/// bits, all in polynomial 0.
///
/// The hint encodes as `omega` index bytes then `k` cumulative cut bytes.
/// `Hint::bit_unpack` accepts indices `0..omega` with every cut at `omega`,
/// and that is the maximum work it can be made to do: one bit set per unit of
/// weight, plus one ordering comparison per adjacent index pair within a
/// polynomial, which concentrating in a single polynomial maximizes.
fn max_hint_weight_signature<P: MlDsaParams>(rng: &mut StdRng) -> Vec<u8> {
    let (omega, k) = (P::Omega::USIZE, P::K::USIZE);
    let mut sig = sign_random::<P>(rng);
    let hint = sig.len() - (omega + k);
    // Indices 0..omega, strictly increasing, all in polynomial 0.
    for (i, b) in sig[hint..hint + omega].iter_mut().enumerate() {
        *b = i as u8;
    }
    // Cumulative cuts: polynomial 0 takes all omega, the rest take none. The
    // last cut is the total weight, so no padding bytes remain to zero.
    sig[hint + omega..].fill(omega as u8);
    debug_assert_eq!(*sig.last().expect("non-empty signature") as usize, omega);
    // A construction the decoder rejects would surface as an `unwrap` panic
    // deep inside the runner; fail here with something readable instead.
    assert!(
        Signature::<P>::try_from(sig.as_slice()).is_ok(),
        "constructed maximum-hint signature must still decode"
    );
    sig
}

fn decode_vk_sample<P: MlDsaParams>(rng: &mut StdRng) -> MlDsaDecodeVerifyingKeySample {
    let sk = random_signing_key::<P>(rng);
    let vk = sk.expanded_key().verifying_key();
    MlDsaDecodeVerifyingKeySample {
        bytes: vk.encode().to_vec(),
    }
}

fn decode_sig_sample<P: MlDsaParams>(rng: &mut StdRng) -> MlDsaDecodeSignatureSample {
    MlDsaDecodeSignatureSample {
        bytes: sign_random::<P>(rng),
    }
}

fn verify_sample<P: MlDsaParams>(rng: &mut StdRng, total_len: u64) -> MlDsaVerifySigSample<P> {
    let sk = random_signing_key::<P>(rng);
    let vk = sk.expanded_key().verifying_key();
    // The linear input is the combined message and context length, so the
    // context length is subtracted from the requested total.
    let msg_len = (total_len as usize).saturating_sub(CTX_LEN);
    let mut msg = vec![0u8; msg_len];
    rng.fill_bytes(&mut msg);
    let mut ctx = vec![0u8; CTX_LEN];
    rng.fill_bytes(&mut ctx);
    let sig = sk
        .expanded_key()
        .sign_deterministic(&msg, &ctx)
        .expect("deterministic signing");
    MlDsaVerifySigSample { vk, msg, sig, ctx }
}

macro_rules! impl_ml_dsa_measures {
    (
        $p:ty,
        $decode_vk_measure:ident, $decode_vk_run:ident,
        $decode_sig_measure:ident, $decode_sig_run:ident,
        $verify_measure:ident, $verify_run:ident
    ) => {
        // Constant-cost measurement: decoding (and expanding) a random
        // verifying key.
        pub(crate) struct $decode_vk_measure;

        impl HostCostMeasurement for $decode_vk_measure {
            type Runner = $decode_vk_run;

            fn new_random_case(
                _host: &Host,
                rng: &mut StdRng,
                _input: u64,
            ) -> MlDsaDecodeVerifyingKeySample {
                decode_vk_sample::<$p>(rng)
            }
        }

        // Constant-cost measurement: unpacking and validating the hint and
        // response vectors of a signature.
        pub(crate) struct $decode_sig_measure;

        impl HostCostMeasurement for $decode_sig_measure {
            type Runner = $decode_sig_run;

            fn new_random_case(
                _host: &Host,
                rng: &mut StdRng,
                _input: u64,
            ) -> MlDsaDecodeSignatureSample {
                decode_sig_sample::<$p>(rng)
            }

            // Decode cost increases monotonically with hint weight, so the
            // worst case is a signature at the maximum weight `omega`.
            fn new_worst_case(
                _host: &Host,
                rng: &mut StdRng,
                _input: u64,
            ) -> MlDsaDecodeSignatureSample {
                MlDsaDecodeSignatureSample {
                    bytes: max_hint_weight_signature::<$p>(rng),
                }
            }
        }

        // Linear measurement: verification of a random signature, with the
        // message length scaling with the input (the SHAKE-256 absorption
        // computing `mu` is the linear component).
        pub(crate) struct $verify_measure;

        impl HostCostMeasurement for $verify_measure {
            type Runner = $verify_run;
            const INPUT_BASE_SIZE: u64 = 0;
            const STEP_SIZE: u64 = SHAKE256_RATE;

            fn new_random_case(
                _host: &Host,
                rng: &mut StdRng,
                input: u64,
            ) -> MlDsaVerifySigSample<$p> {
                let size = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
                verify_sample::<$p>(rng, size)
            }
        }
    };
}

impl_ml_dsa_measures!(
    MlDsa44,
    MlDsa44DecodeVerifyingKeyMeasure,
    MlDsa44DecodeVerifyingKeyRun,
    MlDsa44DecodeSignatureMeasure,
    MlDsa44DecodeSignatureRun,
    VerifyMlDsa44SigMeasure,
    VerifyMlDsa44SigRun
);
impl_ml_dsa_measures!(
    MlDsa65,
    MlDsa65DecodeVerifyingKeyMeasure,
    MlDsa65DecodeVerifyingKeyRun,
    MlDsa65DecodeSignatureMeasure,
    MlDsa65DecodeSignatureRun,
    VerifyMlDsa65SigMeasure,
    VerifyMlDsa65SigRun
);
impl_ml_dsa_measures!(
    MlDsa87,
    MlDsa87DecodeVerifyingKeyMeasure,
    MlDsa87DecodeVerifyingKeyRun,
    MlDsa87DecodeSignatureMeasure,
    MlDsa87DecodeSignatureRun,
    VerifyMlDsa87SigMeasure,
    VerifyMlDsa87SigRun
);
