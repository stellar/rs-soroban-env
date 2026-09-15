//! Tests for the verify_sig_ml_dsa_{44,65,87} host functions.
//!
//! Everything here is self-contained: key pairs come from seeded
//! deterministic keygen and signatures from `sign_deterministic`, so no RNG
//! and no vector files are involved. Conformance against the NIST ACVP and
//! Wycheproof vector sets is tested separately.
//!
//! The host functions are gated at protocol 30 ("next"), the protocol
//! CAP-0087 targets. Under default features the env/ledger protocol is 29, so
//! calling them traps with Context/IndexBounds. Behavior tests therefore
//! early-return unless the ledger protocol supports ML-DSA (i.e. they fully
//! run under `--features next`). Observation recording is disabled under
//! `next` (see observe.rs), so only the protocol-gate test is observed.

use crate::{
    xdr::{ScErrorCode, ScErrorType},
    Env, EnvBase, Host, HostError,
};
use ml_dsa::{MlDsa44, MlDsa65, MlDsa87, MlDsaParams, SigningKey};

const ML_DSA_MIN_PROTOCOL: u32 = 30;

const VARIANTS: [&str; 3] = ["ML-DSA-44", "ML-DSA-65", "ML-DSA-87"];

fn ml_dsa_enabled(host: &Host) -> bool {
    host.get_ledger_protocol_version().unwrap() >= ML_DSA_MIN_PROTOCOL
}

/// A deterministic (encoded verifying key, encoded signature) pair over
/// `msg` and `ctx`. Both keygen and signing are seeded, so the fixture is
/// stable across runs and platforms.
fn sign_fixture<P: MlDsaParams>(seed: u8, msg: &[u8], ctx: &[u8]) -> (Vec<u8>, Vec<u8>) {
    let sk = SigningKey::<P>::from_seed(&[seed; 32].into());
    let expanded = sk.expanded_key();
    let sig = expanded
        .sign_deterministic(msg, ctx)
        .expect("deterministic signing");
    (
        expanded.verifying_key().encode().to_vec(),
        sig.encode().to_vec(),
    )
}

/// `sign_fixture` dispatched on the parameter set name, so tests can loop
/// over variants without repeating themselves.
fn fixture_for(parameter_set: &str, seed: u8, msg: &[u8], ctx: &[u8]) -> (Vec<u8>, Vec<u8>) {
    match parameter_set {
        "ML-DSA-44" => sign_fixture::<MlDsa44>(seed, msg, ctx),
        "ML-DSA-65" => sign_fixture::<MlDsa65>(seed, msg, ctx),
        "ML-DSA-87" => sign_fixture::<MlDsa87>(seed, msg, ctx),
        other => panic!("unknown parameter set {other}"),
    }
}

fn host_verify_ml_dsa(
    host: &Host,
    parameter_set: &str,
    pk: &[u8],
    msg: &[u8],
    sig: &[u8],
    ctx: &[u8],
) -> Result<crate::Void, HostError> {
    let pk_obj = host.bytes_new_from_slice(pk)?;
    let msg_obj = host.bytes_new_from_slice(msg)?;
    let sig_obj = host.bytes_new_from_slice(sig)?;
    let ctx_obj = host.bytes_new_from_slice(ctx)?;
    match parameter_set {
        "ML-DSA-44" => host.verify_sig_ml_dsa_44(pk_obj, msg_obj, sig_obj, ctx_obj),
        "ML-DSA-65" => host.verify_sig_ml_dsa_65(pk_obj, msg_obj, sig_obj, ctx_obj),
        "ML-DSA-87" => host.verify_sig_ml_dsa_87(pk_obj, msg_obj, sig_obj, ctx_obj),
        other => panic!("unknown parameter set {other}"),
    }
}

fn assert_err_type(res: Result<crate::Void, HostError>, ty: ScErrorType, ctx: &str) {
    let err = res.expect_err(&format!("expected {ty:?} error: {ctx}"));
    assert!(
        err.error.is_type(ty),
        "expected {ty:?}, got {err:?} ({ctx})"
    );
}

/// The host functions are protocol-gated at 30: below that they must trap
/// with Context/IndexBounds, at or above they verify successfully. This test
/// is meaningful (and observed) under default features, and exercises the
/// success path under `--features next`.
#[test]
fn ml_dsa_protocol_gate() {
    let host = observe_host!(Host::test_host());
    let msg = b"soroban ml-dsa protocol gate";
    let (pk, sig) = fixture_for("ML-DSA-44", 1, msg, b"");
    let res = host_verify_ml_dsa(&host, "ML-DSA-44", &pk, msg, &sig, b"");
    if ml_dsa_enabled(&host) {
        assert!(res.is_ok(), "expected success at protocol >= 30: {res:?}");
    } else {
        let err = res.expect_err("expected protocol gate trap below protocol 30");
        assert!(err.error.is_type(ScErrorType::Context));
        assert!(err.error.is_code(ScErrorCode::IndexBounds));
    }
}

/// A freshly signed message verifies, for every parameter set, with both an
/// empty and a non-empty context string.
#[test]
fn ml_dsa_verify_happy_path() {
    let host = Host::test_host();
    if !ml_dsa_enabled(&host) {
        return;
    }
    for variant in VARIANTS {
        for (seed, ctx) in [(1u8, &b""[..]), (2u8, &b"soroban-domain-separator"[..])] {
            let msg = b"attestation payload";
            let (pk, sig) = fixture_for(variant, seed, msg, ctx);
            assert!(
                host_verify_ml_dsa(&host, variant, &pk, msg, &sig, ctx).is_ok(),
                "{variant} should verify (ctx len {})",
                ctx.len()
            );
        }
    }
}

/// An empty message and a maximum-length (255-byte) context are both valid
/// inputs per the FIPS 204 external interface.
#[test]
fn ml_dsa_boundary_inputs() {
    let host = Host::test_host();
    if !ml_dsa_enabled(&host) {
        return;
    }
    for variant in VARIANTS {
        let max_ctx = [0xABu8; 255];
        let (pk, sig) = fixture_for(variant, 3, b"", &max_ctx);
        assert!(
            host_verify_ml_dsa(&host, variant, &pk, b"", &sig, &max_ctx).is_ok(),
            "{variant} should accept an empty message and a 255-byte context"
        );
    }
}

/// Corrupt one input dimension at a time, starting from a known-good
/// fixture, and check each is rejected with the right error type.
#[test]
fn ml_dsa_error_paths() {
    let host = Host::test_host();
    if !ml_dsa_enabled(&host) {
        return;
    }
    let msg = b"attestation payload";
    let ctx = b"soroban-domain-separator";
    for variant in VARIANTS {
        let (pk, sig) = fixture_for(variant, 4, msg, ctx);

        // Wrong verifying key length (one byte short).
        assert_err_type(
            host_verify_ml_dsa(&host, variant, &pk[..pk.len() - 1], msg, &sig, ctx),
            ScErrorType::Crypto,
            "truncated pk",
        );

        // Wrong signature length (one extra byte).
        let mut long_sig = sig.clone();
        long_sig.push(0);
        assert_err_type(
            host_verify_ml_dsa(&host, variant, &pk, msg, &long_sig, ctx),
            ScErrorType::Crypto,
            "oversized sig",
        );

        // Context longer than 255 bytes: a length error, so Object, not Crypto.
        assert_err_type(
            host_verify_ml_dsa(&host, variant, &pk, msg, &sig, &[0u8; 256]),
            ScErrorType::Crypto,
            "ctx > 255",
        );

        // Bit-flipped message.
        let mut bad_msg = *msg;
        bad_msg[0] ^= 0x01;
        assert_err_type(
            host_verify_ml_dsa(&host, variant, &pk, &bad_msg, &sig, ctx),
            ScErrorType::Crypto,
            "bit-flipped msg",
        );

        // Bit-flipped signature (flip a byte in c_tilde, the leading bytes).
        let mut bad_sig = sig.clone();
        bad_sig[0] ^= 0x01;
        assert_err_type(
            host_verify_ml_dsa(&host, variant, &pk, msg, &bad_sig, ctx),
            ScErrorType::Crypto,
            "bit-flipped sig",
        );

        // Wrong context (valid length, different content).
        let mut bad_ctx = ctx.to_vec();
        bad_ctx[0] ^= 0x01;
        assert_err_type(
            host_verify_ml_dsa(&host, variant, &pk, msg, &sig, &bad_ctx),
            ScErrorType::Crypto,
            "wrong ctx",
        );

        // Signature from a different key.
        let (_, other_sig) = fixture_for(variant, 5, msg, ctx);
        assert_err_type(
            host_verify_ml_dsa(&host, variant, &pk, msg, &other_sig, ctx),
            ScErrorType::Crypto,
            "sig from another key",
        );
    }
}

/// Keys and signatures from one variant must be rejected by another
/// variant's host function (sizes differ, so Crypto/InvalidInput).
#[test]
fn ml_dsa_cross_variant_confusion() {
    let host = Host::test_host();
    if !ml_dsa_enabled(&host) {
        return;
    }
    let msg = b"attestation payload";
    let (pk44, sig44) = fixture_for("ML-DSA-44", 6, msg, b"");
    let (pk65, sig65) = fixture_for("ML-DSA-65", 6, msg, b"");
    // 65-sized inputs into the _44 function.
    assert_err_type(
        host_verify_ml_dsa(&host, "ML-DSA-44", &pk65, msg, &sig65, b""),
        ScErrorType::Crypto,
        "65 inputs into _44",
    );
    // 44-sized inputs into the _65 function.
    assert_err_type(
        host_verify_ml_dsa(&host, "ML-DSA-65", &pk44, msg, &sig44, b""),
        ScErrorType::Crypto,
        "44 inputs into _65",
    );
}

/// With a tiny CPU budget the verification must fail with Budget/ExceededLimit
/// (charges happen before the expensive work).
#[test]
fn ml_dsa_budget_exhaustion() -> Result<(), HostError> {
    use crate::budget::AsBudget;
    let host = Host::test_host();
    if !ml_dsa_enabled(&host) {
        return Ok(());
    }
    let msg = b"attestation payload";
    let (pk, sig) = fixture_for("ML-DSA-65", 7, msg, b"");
    host.as_budget().reset_limits(10_000, 10_000)?;
    let res = host_verify_ml_dsa(&host, "ML-DSA-65", &pk, msg, &sig, b"");
    let err = res.expect_err("expected budget exhaustion");
    assert!(err.error.is_type(ScErrorType::Budget));
    Ok(())
}
