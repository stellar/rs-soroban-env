use crate::{
    crypto::poseidon::poseidon2::Poseidon2,
    testutils::crypto::{from_hex, random_scalar, vec_of_vec_to_vecobj},
    xdr::{ScErrorCode, ScErrorType},
    Env, Host, HostError, Symbol, U32Val, VecObject,
};
use ark_bls12_381::Fr as BlsScalar;
use ark_bn254::Fr as BnScalar;
use rand::{rngs::StdRng, SeedableRng};
use std::cmp::Ordering;

#[test]
fn test_poseidon2_bn254_hostfn_success() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    // Use the BN254 3-element Poseidon2 parameters
    use super::poseidon2_instance_bn254::POSEIDON2_BN254_PARAMS;
    let params = &**POSEIDON2_BN254_PARAMS;

    // Create test input
    let input: Vec<BnScalar> = vec![BnScalar::from(0), BnScalar::from(1), BnScalar::from(2)];
    // Convert inputs to host expected format
    let input_vecobj = host.metered_scalar_vec_to_vecobj(input)?;
    let mat_internal_diag_m_1_vecobj =
        host.metered_scalar_vec_to_vecobj(params.mat_internal_diag_m_1.clone())?;
    let rc_vecobj = vec_of_vec_to_vecobj(&host, &params.round_constants)?;

    // Call the host function
    // Note: full rounds are applied at both the beginning and the end, thus `rounds_f = 2 * rounds_f_beginning`
    let field_symbol = Symbol::try_from_small_str("BN254")?;
    let result = host.poseidon2_permutation(
        input_vecobj,
        field_symbol,
        U32Val::from(params.t as u32),
        U32Val::from(params.d as u32),
        U32Val::from((params.rounds_f_beginning * 2) as u32),
        U32Val::from(params.rounds_p as u32),
        mat_internal_diag_m_1_vecobj,
        rc_vecobj,
    )?;

    let expected: Vec<BnScalar> = vec![
        from_hex("0x0bb61d24daca55eebcb1929a82650f328134334da98ea4f847f760054f4a3033"),
        from_hex("0x303b6f7c86d043bfcbcc80214f26a30277a15d3f74ca654992defe7ff8d03570"),
        from_hex("0x1ed25194542b12eef8617361c3ba7c52e660b145994427cc86296242cf766ec8"),
    ];
    let expected = host.metered_scalar_vec_to_vecobj(expected)?;
    assert_eq!(
        host.obj_cmp(result.into(), expected.into())?,
        Ordering::Equal as i64
    );
    Ok(())
}

#[test]
fn test_poseidon2_bls12_381_hostfn_success() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    // Use the BLS12-381 3-element Poseidon2 parameters
    use super::poseidon2_instance_bls12::POSEIDON2_BLS_3_PARAMS;
    let params = &**POSEIDON2_BLS_3_PARAMS;

    // Create test input
    let input: Vec<BlsScalar> = vec![BlsScalar::from(0), BlsScalar::from(1), BlsScalar::from(2)];
    // Convert inputs to host expected format
    let input_vecobj = host.metered_scalar_vec_to_vecobj(input)?;
    let mat_internal_diag_m_1_vecobj =
        host.metered_scalar_vec_to_vecobj(params.mat_internal_diag_m_1.clone())?;
    let rc_vecobj = vec_of_vec_to_vecobj(&host, &params.round_constants)?;

    // Call the host function
    // Note: full rounds are applied at both the beginning and the end, thus `rounds_f = 2 * rounds_f_beginning`
    let field_symbol = Symbol::try_from_small_str("BLS12_381")?;
    let result = host.poseidon2_permutation(
        input_vecobj,
        field_symbol,
        U32Val::from(params.t as u32),
        U32Val::from(params.d as u32),
        U32Val::from((params.rounds_f_beginning * 2) as u32),
        U32Val::from(params.rounds_p as u32),
        mat_internal_diag_m_1_vecobj,
        rc_vecobj,
    )?;

    let expected: Vec<BlsScalar> = vec![
        from_hex("0x1b152349b1950b6a8ca75ee4407b6e26ca5cca5650534e56ef3fd45761fbf5f0"),
        from_hex("0x4c5793c87d51bdc2c08a32108437dc0000bd0275868f09ebc5f36919af5b3891"),
        from_hex("0x1fc8ed171e67902ca49863159fe5ba6325318843d13976143b8125f08b50dc6b"),
    ];
    let expected = host.metered_scalar_vec_to_vecobj(expected)?;
    assert_eq!(
        host.obj_cmp(result.into(), expected.into())?,
        Ordering::Equal as i64
    );
    Ok(())
}

// Helper function for testing invalid inputs - returns the error for validation
fn test_poseidon2_invalid<F>(host: &Host, modify_fn: F) -> Result<VecObject, HostError>
where
    F: FnOnce(
        &Host,
        &mut VecObject,
        &mut Symbol,
        &mut U32Val,
        &mut U32Val,
        &mut U32Val,
        &mut U32Val,
        &mut VecObject,
        &mut VecObject,
    ) -> Result<(), HostError>,
{
    use super::poseidon2_instance_bn254::POSEIDON2_BN254_PARAMS;
    let params = &**POSEIDON2_BN254_PARAMS;

    // Set up default valid inputs
    let input: Vec<BnScalar> = vec![BnScalar::from(0), BnScalar::from(1), BnScalar::from(2)];
    let mut input_vecobj = host.metered_scalar_vec_to_vecobj(input)?;
    let mut mat_internal_diag_m_1_vecobj =
        host.metered_scalar_vec_to_vecobj(params.mat_internal_diag_m_1.clone())?;
    let mut rc_vecobj = vec_of_vec_to_vecobj(host, &params.round_constants)?;

    let mut field_symbol = Symbol::try_from_small_str("BN254")?;
    let mut t = U32Val::from(params.t as u32);
    let mut d = U32Val::from(params.d as u32);
    let mut rounds_f = U32Val::from((params.rounds_f_beginning * 2) as u32);
    let mut rounds_p = U32Val::from(params.rounds_p as u32);

    // Apply modifications
    modify_fn(
        host,
        &mut input_vecobj,
        &mut field_symbol,
        &mut t,
        &mut d,
        &mut rounds_f,
        &mut rounds_p,
        &mut mat_internal_diag_m_1_vecobj,
        &mut rc_vecobj,
    )?;

    // Call the host function
    host.poseidon2_permutation(
        input_vecobj,
        field_symbol,
        t,
        d,
        rounds_f,
        rounds_p,
        mat_internal_diag_m_1_vecobj,
        rc_vecobj,
    )
}

#[test]
fn test_poseidon2_invalid_field_symbol() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon2_invalid(
        &host,
        |_host, _input, field, _t, _d, _rounds_f, _rounds_p, _mat, _rc| {
            *field = Symbol::try_from_small_str("INVALID")?;
            Ok(())
        },
    );

    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));
    Ok(())
}

#[test]
fn test_poseidon2_input_length_mismatch() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon2_invalid(
        &host,
        |host, input, _field, _t, _d, _rounds_f, _rounds_p, _mat, _rc| {
            // Create input with wrong length (2 instead of 3)
            let wrong_input: Vec<BnScalar> = vec![BnScalar::from(0), BnScalar::from(1)];
            *input = host.metered_scalar_vec_to_vecobj(wrong_input)?;
            Ok(())
        },
    );

    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));
    Ok(())
}

#[test]
fn test_poseidon2_mat_internal_diag_m_1_dimension_mismatch() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon2_invalid(
        &host,
        |host, _input, _field, t, _d, _rounds_f, _rounds_p, mat, _rc| {
            // Create mat_internal_diag_m_1 with wrong length
            let wrong_mat: Vec<BnScalar> = vec![BnScalar::from(1), BnScalar::from(2)];
            *mat = host.metered_scalar_vec_to_vecobj(wrong_mat)?;
            *t = U32Val::from(2);
            Ok(())
        },
    );

    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));
    Ok(())
}

#[test]
fn test_poseidon2_round_constants_wrong_dimensions() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon2_invalid(
        &host,
        |host, _input, _field, _t, _d, _rounds_f, _rounds_p, _mat, rc| {
            // Create round constants with wrong dimensions (2x3 instead of correct size)
            let wrong_rc: Vec<Vec<BnScalar>> = vec![
                vec![BnScalar::from(1), BnScalar::from(2), BnScalar::from(3)],
                vec![BnScalar::from(4), BnScalar::from(5), BnScalar::from(6)],
            ];
            *rc = vec_of_vec_to_vecobj(host, &wrong_rc)?;
            Ok(())
        },
    );

    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));
    Ok(())
}

#[test]
fn test_poseidon2_round_constants_rows_different_sizes() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon2_invalid(
        &host,
        |host, _input, _field, _t, _d, _rounds_f, _rounds_p, _mat, rc| {
            // Create round constants with rows of different sizes
            let wrong_rc: Vec<Vec<BnScalar>> = vec![
                vec![BnScalar::from(1), BnScalar::from(2), BnScalar::from(3)],
                vec![BnScalar::from(4), BnScalar::from(5)], // Wrong size
                vec![BnScalar::from(6), BnScalar::from(7), BnScalar::from(8)],
            ];
            *rc = vec_of_vec_to_vecobj(host, &wrong_rc)?;
            Ok(())
        },
    );

    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));
    Ok(())
}

#[test]
fn test_poseidon2_rounds_inconsistent_with_round_constants() -> Result<(), HostError> {
    let host = Host::test_host();

    use super::poseidon2_instance_bn254::POSEIDON2_BN254_PARAMS;
    let params = &**POSEIDON2_BN254_PARAMS;
    let original_rounds_f = (params.rounds_f_beginning * 2) as u32;
    let original_rounds_p = params.rounds_p as u32;

    // Test case 1: rounds_f decreased by 2
    let result = test_poseidon2_invalid(
        &host,
        |_host, _input, _field, _t, _d, rounds_f, _rounds_p, _mat, _rc| {
            *rounds_f = U32Val::from(original_rounds_f - 2);
            Ok(())
        },
    );
    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));

    // Test case 2: rounds_f increased by 2
    let result = test_poseidon2_invalid(
        &host,
        |_host, _input, _field, _t, _d, rounds_f, _rounds_p, _mat, _rc| {
            *rounds_f = U32Val::from(original_rounds_f + 2);
            Ok(())
        },
    );
    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));

    // Test case 3: rounds_p decreased by 2
    let result = test_poseidon2_invalid(
        &host,
        |_host, _input, _field, _t, _d, _rounds_f, rounds_p, _mat, _rc| {
            *rounds_p = U32Val::from(original_rounds_p - 2);
            Ok(())
        },
    );
    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));

    // Test case 4: rounds_p increased by 2
    let result = test_poseidon2_invalid(
        &host,
        |_host, _input, _field, _t, _d, _rounds_f, rounds_p, _mat, _rc| {
            *rounds_p = U32Val::from(original_rounds_p + 2);
            Ok(())
        },
    );
    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));

    // Test case 5: rounds_f decreased by 1 and rounds_p increased by 1
    // This should trigger the rounds_f not even condition
    let result = test_poseidon2_invalid(
        &host,
        |_host, _input, _field, _t, _d, rounds_f, rounds_p, _mat, _rc| {
            *rounds_f = U32Val::from(original_rounds_f - 1);
            *rounds_p = U32Val::from(original_rounds_p + 1);
            Ok(())
        },
    );
    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));

    Ok(())
}

#[test]
fn test_poseidon2_invalid_degree() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon2_invalid(
        &host,
        |_host, _input, _field, _t, d, _rounds_f, _rounds_p, _mat, _rc| {
            // Use an invalid s-box degree (not in SUPPORTED_SBOX_DEGREES)
            *d = U32Val::from(7);
            Ok(())
        },
    );

    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));
    Ok(())
}

#[test]
fn test_poseidon2_invalid_t() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon2_invalid(
        &host,
        |_host, _input, _field, t, _d, _rounds_f, _rounds_p, _mat, _rc| {
            // Set t to 0 or some invalid value
            *t = U32Val::from(0);
            Ok(())
        },
    );

    assert!(result.is_err());
    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));
    Ok(())
}

#[test]
fn test_poseidon2_mismatching_field_types() -> Result<(), HostError> {
    let host = Host::test_host();

    // Use BN254 parameters
    use super::poseidon2_instance_bn254::POSEIDON2_BN254_PARAMS;
    let bn_params = &**POSEIDON2_BN254_PARAMS;

    // Create input that can be interpreted in both fields (small values)
    let input_bn: Vec<BnScalar> = vec![BnScalar::from(1), BnScalar::from(2), BnScalar::from(3)];
    let input_bls: Vec<BlsScalar> =
        vec![BlsScalar::from(1), BlsScalar::from(2), BlsScalar::from(3)];

    // Run Poseidon2 with BN254 field
    let input_vecobj_bn = host.metered_scalar_vec_to_vecobj(input_bn)?;
    let mat_vecobj = host.metered_scalar_vec_to_vecobj(bn_params.mat_internal_diag_m_1.clone())?;
    let rc_vecobj = vec_of_vec_to_vecobj(&host, &bn_params.round_constants)?;

    let result_bn = host.poseidon2_permutation(
        input_vecobj_bn,
        Symbol::try_from_small_str("BN254")?,
        U32Val::from(bn_params.t as u32),
        U32Val::from(bn_params.d as u32),
        U32Val::from((bn_params.rounds_f_beginning * 2) as u32),
        U32Val::from(bn_params.rounds_p as u32),
        mat_vecobj,
        rc_vecobj,
    )?;

    // Run Poseidon2 with BLS12_381 field using the SAME constants
    let input_vecobj_bls = host.metered_scalar_vec_to_vecobj(input_bls)?;
    let mat_vecobj_bls =
        host.metered_scalar_vec_to_vecobj(bn_params.mat_internal_diag_m_1.clone())?;
    let rc_vecobj_bls = vec_of_vec_to_vecobj(&host, &bn_params.round_constants)?;

    let result_bls = host.poseidon2_permutation(
        input_vecobj_bls,
        Symbol::try_from_small_str("BLS12_381")?,
        U32Val::from(bn_params.t as u32),
        U32Val::from(bn_params.d as u32),
        U32Val::from((bn_params.rounds_f_beginning * 2) as u32),
        U32Val::from(bn_params.rounds_p as u32),
        mat_vecobj_bls,
        rc_vecobj_bls,
    )?;

    // The results should NOT be equal because the field modulus operations are different
    assert_ne!(
        host.obj_cmp(result_bn.into(), result_bls.into())?,
        Ordering::Equal as i64,
        "Poseidon2 results should differ when using different field types with same constants"
    );

    Ok(())
}

#[cfg(test)]
mod poseidon2_tests_bls12 {
    use super::*;
    use crate::test::poseidon::poseidon2_instance_bls12::{
        POSEIDON2_BLS_2_PARAMS, POSEIDON2_BLS_3_PARAMS, POSEIDON2_BLS_4_PARAMS,
        POSEIDON2_BLS_8_PARAMS,
    };

    type Scalar = BlsScalar;

    static TESTRUNS: usize = 5;

    #[test]
    fn consistent_perm() {
        let host = Host::test_host();
        let mut rng = StdRng::from_seed([0xff; 32]);
        let instances = vec![
            Poseidon2::new((**POSEIDON2_BLS_2_PARAMS).clone()),
            Poseidon2::new((**POSEIDON2_BLS_3_PARAMS).clone()),
            Poseidon2::new((**POSEIDON2_BLS_4_PARAMS).clone()),
            Poseidon2::new((**POSEIDON2_BLS_8_PARAMS).clone()),
        ];
        for instance in instances {
            let t = instance.params.t;
            for _ in 0..TESTRUNS {
                let input1: Vec<Scalar> = (0..t).map(|_| random_scalar(&mut rng)).collect();

                let mut input2: Vec<Scalar>;
                loop {
                    input2 = (0..t).map(|_| random_scalar(&mut rng)).collect();
                    if input1 != input2 {
                        break;
                    }
                }

                let perm1 = instance.permutation(&host, &input1).unwrap();
                let perm2 = instance.permutation(&host, &input1).unwrap();
                let perm3 = instance.permutation(&host, &input2).unwrap();
                assert_eq!(perm1, perm2);
                assert_ne!(perm1, perm3);
            }
        }
    }

    #[test]
    fn kats() {
        let host = Host::test_host();
        let poseidon2_2 = Poseidon2::new((**POSEIDON2_BLS_2_PARAMS).clone());
        let mut input_2: Vec<Scalar> = vec![];
        for i in 0..poseidon2_2.params.t {
            input_2.push(Scalar::from(i as u64));
        }
        let perm_2 = poseidon2_2.permutation(&host, &input_2).unwrap();
        assert_eq!(
            perm_2[0],
            from_hex("0x73c46dd530e248a87b61d19e67fa1b4ed30fc3d09f16531fe189fb945a15ce4e")
        );
        assert_eq!(
            perm_2[1],
            from_hex("0x1f0e305ee21c9366d5793b80251405032a3fee32b9dd0b5f4578262891b043b4")
        );

        let poseidon2_3 = Poseidon2::new((**POSEIDON2_BLS_3_PARAMS).clone());
        let mut input_3: Vec<Scalar> = vec![];
        for i in 0..poseidon2_3.params.t {
            input_3.push(Scalar::from(i as u64));
        }
        let perm_3 = poseidon2_3.permutation(&host, &input_3).unwrap();
        assert_eq!(
            perm_3[0],
            from_hex("0x1b152349b1950b6a8ca75ee4407b6e26ca5cca5650534e56ef3fd45761fbf5f0")
        );
        assert_eq!(
            perm_3[1],
            from_hex("0x4c5793c87d51bdc2c08a32108437dc0000bd0275868f09ebc5f36919af5b3891")
        );
        assert_eq!(
            perm_3[2],
            from_hex("0x1fc8ed171e67902ca49863159fe5ba6325318843d13976143b8125f08b50dc6b")
        );

        let poseidon2_4 = Poseidon2::new((**POSEIDON2_BLS_4_PARAMS).clone());
        let mut input_4: Vec<Scalar> = vec![];
        for i in 0..poseidon2_4.params.t {
            input_4.push(Scalar::from(i as u64));
        }
        let perm_4 = poseidon2_4.permutation(&host, &input_4).unwrap();
        assert_eq!(
            perm_4[0],
            from_hex("0x28ff6c4edf9768c08ae26290487e93449cc8bc155fc2fad92a344adceb3ada6d")
        );
        assert_eq!(
            perm_4[1],
            from_hex("0x0e56f2b6fad25075aa93560185b70e2b180ed7e269159c507c288b6747a0db2d")
        );
        assert_eq!(
            perm_4[2],
            from_hex("0x6d8196f28da6006bb89b3df94600acdc03d0ba7c2b0f3f4409a54c1db6bf30d0")
        );
        assert_eq!(
            perm_4[3],
            from_hex("0x07cfb49540ee456cce38b8a7d1a930a57ffc6660737f6589ef184c5e15334e36")
        );
    }
}

#[cfg(test)]
mod poseidon2_tests_bn254 {
    use super::*;
    use crate::test::poseidon::poseidon2_instance_bn254::POSEIDON2_BN254_PARAMS;

    type Scalar = BnScalar;

    static TESTRUNS: usize = 5;

    #[test]
    fn consistent_perm() {
        let host = Host::test_host();
        let mut rng = StdRng::from_seed([0xff; 32]);
        let poseidon2 = Poseidon2::new((**POSEIDON2_BN254_PARAMS).clone());
        let t = poseidon2.params.t;
        for _ in 0..TESTRUNS {
            let input1: Vec<Scalar> = (0..t).map(|_| random_scalar(&mut rng)).collect();

            let mut input2: Vec<Scalar>;
            loop {
                input2 = (0..t).map(|_| random_scalar(&mut rng)).collect();
                if input1 != input2 {
                    break;
                }
            }

            let perm1 = poseidon2.permutation(&host, &input1).unwrap();
            let perm2 = poseidon2.permutation(&host, &input1).unwrap();
            let perm3 = poseidon2.permutation(&host, &input2).unwrap();
            assert_eq!(perm1, perm2);
            assert_ne!(perm1, perm3);
        }
    }

    #[test]
    fn kats() {
        let host = Host::test_host();
        let poseidon2 = Poseidon2::new((**POSEIDON2_BN254_PARAMS).clone());
        let mut input: Vec<Scalar> = vec![];
        for i in 0..poseidon2.params.t {
            input.push(Scalar::from(i as u64));
        }
        let perm = poseidon2.permutation(&host, &input).unwrap();
        assert_eq!(
            perm[0],
            from_hex("0x0bb61d24daca55eebcb1929a82650f328134334da98ea4f847f760054f4a3033")
        );
        assert_eq!(
            perm[1],
            from_hex("0x303b6f7c86d043bfcbcc80214f26a30277a15d3f74ca654992defe7ff8d03570")
        );
        assert_eq!(
            perm[2],
            from_hex("0x1ed25194542b12eef8617361c3ba7c52e660b145994427cc86296242cf766ec8")
        );
    }
}
