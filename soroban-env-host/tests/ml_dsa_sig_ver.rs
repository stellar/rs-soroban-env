use soroban_env_common::EnvBase;
use soroban_env_host::{budget::AsBudget, BytesObject, Env, Host, HostError, Void};
use wycheproof::{
    mldsa_verify::{TestName, TestSet},
    TestResult,
};

const ML_DSA_MIN_PROTOCOL: u32 = 30;

type VerifyFn =
    fn(&Host, BytesObject, BytesObject, BytesObject, BytesObject) -> Result<Void, HostError>;

// Runs the Wycheproof ML-DSA verification vectors end-to-end through the host
// function for one parameter set. Covers valid signatures with empty and
// non-empty contexts, modified messages and signatures, invalid hint encodings,
// infinity-norm violations and wrong input lengths.
fn run_wycheproof(name: TestName, verify: VerifyFn) -> Result<(), HostError> {
    if Host::current_test_protocol() < ML_DSA_MIN_PROTOCOL {
        return Ok(());
    }
    let host = Host::test_host();
    let test_set = TestSet::load(name).unwrap();
    let (mut valid, mut invalid) = (0usize, 0usize);
    for test_group in test_set.test_groups {
        let public_key = host.bytes_new_from_slice(&test_group.pubkey)?;
        for test in test_group.tests {
            host.as_budget().reset_default()?;
            let msg = host.bytes_new_from_slice(&test.msg)?;
            let signature = host.bytes_new_from_slice(&test.sig)?;
            let ctx = test.ctx.as_ref().map(|c| c.to_vec()).unwrap_or_default();
            let context = host.bytes_new_from_slice(&ctx)?;

            match verify(&host, public_key, msg, signature, context) {
                Ok(_) => assert_eq!(
                    test.result,
                    TestResult::Valid,
                    "{:?} tc {} unexpectedly verified",
                    name,
                    test.tc_id
                ),
                // `Acceptable` is treated as invalid.
                Err(e) => assert_ne!(
                    test.result,
                    TestResult::Valid,
                    "{:?} tc {} failed to verify: {:?}",
                    name,
                    test.tc_id,
                    e
                ),
            }
            if test.result == TestResult::Valid {
                valid += 1;
            } else {
                invalid += 1;
            }
        }
    }
    assert!(
        valid > 0 && invalid > 0,
        "{:?} covered {valid} valid / {invalid} invalid cases",
        name
    );
    Ok(())
}

#[test]
fn wycheproof_ml_dsa_44() -> Result<(), HostError> {
    run_wycheproof(TestName::MlDsa44Verify, Host::verify_sig_ml_dsa_44)
}

#[test]
fn wycheproof_ml_dsa_65() -> Result<(), HostError> {
    run_wycheproof(TestName::MlDsa65Verify, Host::verify_sig_ml_dsa_65)
}

#[test]
fn wycheproof_ml_dsa_87() -> Result<(), HostError> {
    run_wycheproof(TestName::MlDsa87Verify, Host::verify_sig_ml_dsa_87)
}
