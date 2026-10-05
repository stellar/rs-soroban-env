//! Tests for the verify_sig_ml_dsa_{44,65,87} host functions.
//!
//! Everything here is self-contained: key pairs come from seeded
//! deterministic keygen and signatures from `sign_deterministic`, so no RNG
//! and no vector files are involved. Conformance against the NIST ACVP and
//! Wycheproof vector sets is covered by the vector-driven tests further
//! down: ACVP files are vendored verbatim under src/test/data/ml_dsa/acvp/,
//! Wycheproof comes from the `wycheproof` crate.

use crate::{
    xdr::{ScErrorCode, ScErrorType},
    Env, EnvBase, Host, HostError,
};
use ml_dsa::{MlDsa44, MlDsa65, MlDsa87, MlDsaParams, SigningKey};
use serde::{de::DeserializeOwned, Deserialize};
use wycheproof::{mldsa_verify, TestResult};

const VARIANTS: [&str; 3] = ["ML-DSA-44", "ML-DSA-65", "ML-DSA-87"];

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

/// A freshly signed message verifies, for every parameter set, with both an
/// empty and a non-empty context string.
#[test]
fn ml_dsa_verify_happy_path() {
    let host = observe_host!(Host::test_host());
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
    let host = observe_host!(Host::test_host());
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
    let host = observe_host!(Host::test_host());
    let msg = b"attestation payload";
    let ctx = b"soroban-domain-separator";
    for variant in VARIANTS {
        let (pk, sig) = fixture_for(variant, 4, msg, ctx);

        // Wrong verifying key length (one byte short).
        assert!(
            HostError::result_matches_err(
                host_verify_ml_dsa(&host, variant, &pk[..pk.len() - 1], msg, &sig, ctx),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ),
            "truncated pk"
        );

        // Wrong signature length (one extra byte).
        let mut long_sig = sig.clone();
        long_sig.push(0);
        assert!(
            HostError::result_matches_err(
                host_verify_ml_dsa(&host, variant, &pk, msg, &long_sig, ctx),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ),
            "oversized sig"
        );

        // Context longer than 255 bytes: Crypto/InvalidInput per CAP-0087.
        assert!(
            HostError::result_matches_err(
                host_verify_ml_dsa(&host, variant, &pk, msg, &sig, &[0u8; 256]),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ),
            "ctx > 255"
        );

        // Bit-flipped message.
        let mut bad_msg = *msg;
        bad_msg[0] ^= 0x01;
        assert!(
            HostError::result_matches_err(
                host_verify_ml_dsa(&host, variant, &pk, &bad_msg, &sig, ctx),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ),
            "bit-flipped msg"
        );

        // Bit-flipped signature (flip a byte in c_tilde, the leading bytes).
        let mut bad_sig = sig.clone();
        bad_sig[0] ^= 0x01;
        assert!(
            HostError::result_matches_err(
                host_verify_ml_dsa(&host, variant, &pk, msg, &bad_sig, ctx),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ),
            "bit-flipped sig"
        );

        // Wrong context (valid length, different content).
        let mut bad_ctx = ctx.to_vec();
        bad_ctx[0] ^= 0x01;
        assert!(
            HostError::result_matches_err(
                host_verify_ml_dsa(&host, variant, &pk, msg, &sig, &bad_ctx),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ),
            "wrong ctx"
        );

        // Signature from a different key.
        let (_, other_sig) = fixture_for(variant, 5, msg, ctx);
        assert!(
            HostError::result_matches_err(
                host_verify_ml_dsa(&host, variant, &pk, msg, &other_sig, ctx),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ),
            "sig from another key"
        );
    }
}

/// Keys and signatures from one variant must be rejected by another
/// variant's host function (sizes differ, so Crypto/InvalidInput).
#[test]
fn ml_dsa_cross_variant_confusion() {
    let host = observe_host!(Host::test_host());
    let msg = b"attestation payload";
    let (pk44, sig44) = fixture_for("ML-DSA-44", 6, msg, b"");
    let (pk65, sig65) = fixture_for("ML-DSA-65", 6, msg, b"");
    // 65-sized inputs into the _44 function.
    assert!(
        HostError::result_matches_err(
            host_verify_ml_dsa(&host, "ML-DSA-44", &pk65, msg, &sig65, b""),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ),
        "65 inputs into _44"
    );
    // 44-sized inputs into the _65 function.
    assert!(
        HostError::result_matches_err(
            host_verify_ml_dsa(&host, "ML-DSA-65", &pk44, msg, &sig44, b""),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ),
        "44 inputs into _65"
    );
}

/// With a tiny CPU budget the verification must fail with Budget/ExceededLimit
/// (charges happen before the expensive work).
#[test]
fn ml_dsa_budget_exhaustion() -> Result<(), HostError> {
    use crate::budget::AsBudget;
    let host = observe_host!(Host::test_host());
    let msg = b"attestation payload";
    let (pk, sig) = fixture_for("ML-DSA-65", 7, msg, b"");
    host.as_budget().reset_limits(10_000, 10_000)?;
    let res = host_verify_ml_dsa(&host, "ML-DSA-65", &pk, msg, &sig, b"");
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

// ---------------------------------------------------------------------------
// NIST ACVP ML-DSA vectors, vendored verbatim from usnistgov/ACVP-Server at
// commit a7f283cdc87d2d6dd93c1bac59e5622c5f9f8324:
// gen-val/json-files/ML-DSA-{sigVer,sigGen}-FIPS204/internalProjection.json.
//
// Only groups the host functions can exercise are used: the external
// interface (ML-DSA.Verify) over a pure, not pre-hashed, message. The
// internal-interface and HashML-DSA groups are skipped.
// ---------------------------------------------------------------------------

const ACVP_SIG_VER_PATH: &str =
    "./src/test/data/ml_dsa/acvp/ML-DSA-sigVer-FIPS204/internalProjection.json";
const ACVP_SIG_GEN_PATH: &str =
    "./src/test/data/ml_dsa/acvp/ML-DSA-sigGen-FIPS204/internalProjection.json";

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct AcvpFile<T> {
    test_groups: Vec<AcvpGroup<T>>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct AcvpGroup<T> {
    parameter_set: String,
    signature_interface: Option<String>,
    pre_hash: Option<String>,
    tests: Vec<T>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct AcvpSigVerCase {
    tc_id: u64,
    #[serde(with = "hex::serde")]
    pk: Vec<u8>,
    #[serde(default, with = "hex::serde")]
    message: Vec<u8>,
    #[serde(with = "hex::serde")]
    signature: Vec<u8>,
    #[serde(default, with = "hex::serde")]
    context: Vec<u8>,
    test_passed: bool,
    #[serde(default)]
    reason: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct AcvpSigGenCase {
    tc_id: u64,
    #[serde(with = "hex::serde")]
    pk: Vec<u8>,
    #[serde(default, with = "hex::serde")]
    message: Vec<u8>,
    #[serde(with = "hex::serde")]
    signature: Vec<u8>,
    #[serde(default, with = "hex::serde")]
    context: Vec<u8>,
}

/// Loads a vendored ACVP file and returns the external, pure cases paired
/// with their parameter set.
fn load_acvp<T: DeserializeOwned>(path: &str) -> Vec<(String, T)> {
    let data = std::fs::read(path).unwrap();
    let file: AcvpFile<T> = serde_json::from_slice(&data).unwrap();
    file.test_groups
        .into_iter()
        .filter(|g| {
            g.signature_interface.as_deref() == Some("external")
                && g.pre_hash.as_deref() == Some("pure")
        })
        .flat_map(|g| {
            let parameter_set = g.parameter_set;
            g.tests.into_iter().map(move |t| (parameter_set.clone(), t))
        })
        .collect()
}

/// Every ACVP sigVer case for the external, pure interface. Valid signatures
/// must verify; every invalid one must trap with a Crypto error.
#[test]
fn ml_dsa_acvp_sig_ver_external() {
    let host = observe_host!(Host::test_host());
    host.budget_ref().reset_unlimited().unwrap();
    let cases = load_acvp::<AcvpSigVerCase>(ACVP_SIG_VER_PATH);
    // 15 per parameter set in the pinned file
    assert_eq!(cases.len(), 45);
    for (parameter_set, case) in &cases {
        let res = host_verify_ml_dsa(
            &host,
            parameter_set,
            &case.pk,
            &case.message,
            &case.signature,
            &case.context,
        );
        if case.test_passed {
            assert!(
                res.is_ok(),
                "{parameter_set} sigVer tcId={} expected valid, got {:?} (reason: {})",
                case.tc_id,
                res.err(),
                case.reason
            );
        } else {
            assert!(
                HostError::result_matches_err(
                    res,
                    (ScErrorType::Crypto, ScErrorCode::InvalidInput)
                ),
                "{parameter_set} sigVer tcId={} expected Crypto/InvalidInput (reason: {})",
                case.tc_id,
                case.reason
            );
        }
    }
}

/// The expected signatures of every ACVP sigGen case for the external, pure
/// interface, deterministic and hedged alike. Each is a valid signature, so
/// each must verify.
#[test]
fn ml_dsa_acvp_sig_gen_external() {
    let host = observe_host!(Host::test_host());
    // See ml_dsa_acvp_sig_ver_external for why the budget is uncapped here.
    host.budget_ref().reset_unlimited().unwrap();
    let cases = load_acvp::<AcvpSigGenCase>(ACVP_SIG_GEN_PATH);
    // 15 deterministic and 15 hedged per parameter set in the pinned file.
    assert_eq!(cases.len(), 90);
    for (parameter_set, case) in &cases {
        let res = host_verify_ml_dsa(
            &host,
            parameter_set,
            &case.pk,
            &case.message,
            &case.signature,
            &case.context,
        );
        assert!(
            res.is_ok(),
            "{parameter_set} sigGen tcId={} must verify, got {:?}",
            case.tc_id,
            res.err()
        );
    }
}

// ---------------------------------------------------------------------------
// Wycheproof ML-DSA verify vectors (C2SP/wycheproof), loaded from the
// `wycheproof` crate. Cover malformed encodings, boundary conditions,
// infinity-norm violations of the signer response, and context strings.
// ---------------------------------------------------------------------------

fn run_wycheproof(host: &Host, parameter_set: &str, name: mldsa_verify::TestName) {
    // See ml_dsa_acvp_sig_ver_external for why the budget is uncapped here.
    host.budget_ref().reset_unlimited().unwrap();
    let test_set = mldsa_verify::TestSet::load(name).unwrap();
    let mut count = 0;
    for group in &test_set.test_groups {
        for tc in &group.tests {
            let ctx = tc.ctx.as_deref().map_or(&[][..], Vec::as_slice);
            let res = host_verify_ml_dsa(host, parameter_set, &group.pubkey, &tc.msg, &tc.sig, ctx);
            match tc.result {
                TestResult::Valid => assert!(
                    res.is_ok(),
                    "{parameter_set} wycheproof tcId={} ({}) expected valid, got {:?}",
                    tc.tc_id,
                    tc.comment,
                    res.err()
                ),
                TestResult::Invalid => {
                    assert!(
                        HostError::result_matches_err(
                            res,
                            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
                        ),
                        "{parameter_set} wycheproof tcId={} ({}) expected Crypto/InvalidInput",
                        tc.tc_id,
                        tc.comment
                    );
                }
                // Implementation-defined; either outcome is acceptable.
                TestResult::Acceptable => {}
            }
            count += 1;
        }
    }
    assert!(count > 0, "no wycheproof cases ran for {parameter_set}");
}

#[test]
fn ml_dsa_wycheproof_44() {
    let host = observe_host!(Host::test_host());
    run_wycheproof(&host, "ML-DSA-44", mldsa_verify::TestName::MlDsa44Verify);
}

#[test]
fn ml_dsa_wycheproof_65() {
    let host = observe_host!(Host::test_host());
    run_wycheproof(&host, "ML-DSA-65", mldsa_verify::TestName::MlDsa65Verify);
}

#[test]
fn ml_dsa_wycheproof_87() {
    let host = observe_host!(Host::test_host());
    run_wycheproof(&host, "ML-DSA-87", mldsa_verify::TestName::MlDsa87Verify);
}
