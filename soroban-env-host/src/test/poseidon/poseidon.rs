use crate::{
    crypto::poseidon::poseidon::Poseidon,
    testutils::crypto::{from_hex, random_scalar, vec_of_vec_to_vecobj},
    xdr::{ScErrorCode, ScErrorType},
    Env, Host, HostError, Symbol, U32Val, VecObject,
};
use ark_bls12_381::Fr as BlsScalar;
use ark_bn254::Fr as BnScalar;
use rand::{rngs::StdRng, SeedableRng};
use std::cmp::Ordering;

#[test]
fn test_poseidon_bn254_hostfn_success() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    // Use the BN254 3-element Poseidon parameters
    use super::poseidon_instance_bn254::POSEIDON_BN_PARAMS;
    let params = &**POSEIDON_BN_PARAMS;

    // Create test input
    let input: Vec<BnScalar> = vec![BnScalar::from(0), BnScalar::from(1), BnScalar::from(2)];
    // Convert inputs to host expected format
    let input_vecobj = host.metered_scalar_vec_to_vecobj(input)?;
    let mds_vecobj = vec_of_vec_to_vecobj(&host, &params.mds)?;
    let rc_vecobj = vec_of_vec_to_vecobj(&host, &params.round_constants)?;

    // Call the host function
    // Note: full rounds are applied at both the beginning and the end, thus `rounds_f = 2 * rounds_f_beginning`
    let field_symbol = Symbol::try_from_small_str("BN254")?;
    let result = host.poseidon_permutation(
        input_vecobj,
        field_symbol,
        U32Val::from(params.t as u32),
        U32Val::from(params.d as u32),
        U32Val::from((params.rounds_f_beginning * 2) as u32),
        U32Val::from(params.rounds_p as u32),
        mds_vecobj,
        rc_vecobj,
    )?;

    let expected: Vec<BnScalar> = vec![
        from_hex("0x2677d68d9cfa91f197bf5148b50afac461b6b8340ff119a5217794770baade5f"),
        from_hex("0x21ae9d716173496b62c76ad7deb4654961f64334441bcf77e17a047155a3239f"),
        from_hex("0x008f8e7c73ff20b6a141c48cef73215860acc749b14f0a7887f74950215169c6"),
    ];
    let expected = host.metered_scalar_vec_to_vecobj(expected)?;
    assert_eq!(
        host.obj_cmp(result.into(), expected.into())?,
        Ordering::Equal as i64
    );
    Ok(())
}

#[test]
fn test_poseidon_bls12_381_hostfn_success() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    // Use the BLS12-381 3-element Poseidon parameters
    use super::poseidon_instance_bls12::POSEIDON_BLS_3_PARAMS;
    let params = &**POSEIDON_BLS_3_PARAMS;

    // Create test input
    let input: Vec<BlsScalar> = vec![BlsScalar::from(0), BlsScalar::from(1), BlsScalar::from(2)];
    // Convert inputs to host expected format
    let input_vecobj = host.metered_scalar_vec_to_vecobj(input)?;
    let mds_vecobj = vec_of_vec_to_vecobj(&host, &params.mds)?;
    let rc_vecobj = vec_of_vec_to_vecobj(&host, &params.round_constants)?;

    // Call the host function
    // Note: full rounds are applied at both the beginning and the end, thus `rounds_f = 2 * rounds_f_beginning`
    let field_symbol = Symbol::try_from_small_str("BLS12_381")?;
    let result = host.poseidon_permutation(
        input_vecobj,
        field_symbol,
        U32Val::from(params.t as u32),
        U32Val::from(params.d as u32),
        U32Val::from((params.rounds_f_beginning * 2) as u32),
        U32Val::from(params.rounds_p as u32),
        mds_vecobj,
        rc_vecobj,
    )?;

    let expected: Vec<BlsScalar> = vec![
        from_hex("0x200e6982ac00df8fa65cef1fde9f21373fdbbfd98f2df1eb5fa04f3302ab0397"),
        from_hex("0x2233c9a40d91c1f643b700f836a1ac231c3f3a8d438ad1609355e1b7317a47e5"),
        from_hex("0x2eae6736db3c086ad29938869dedbf969dd9804a58aa228ec467b7d5a08dc765"),
    ];
    let expected = host.metered_scalar_vec_to_vecobj(expected)?;
    assert_eq!(
        host.obj_cmp(result.into(), expected.into())?,
        Ordering::Equal as i64
    );
    Ok(())
}

// Helper function for testing invalid inputs - returns the error for validation
fn test_poseidon_invalid<F>(host: &Host, modify_fn: F) -> Result<VecObject, HostError>
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
    use super::poseidon_instance_bn254::POSEIDON_BN_PARAMS;
    let params = &**POSEIDON_BN_PARAMS;

    // Set up default valid inputs
    let input: Vec<BnScalar> = vec![BnScalar::from(0), BnScalar::from(1), BnScalar::from(2)];
    let mut input_vecobj = host.metered_scalar_vec_to_vecobj(input)?;
    let mut mds_vecobj = vec_of_vec_to_vecobj(host, &params.mds)?;
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
        &mut mds_vecobj,
        &mut rc_vecobj,
    )?;

    // Call the host function
    host.poseidon_permutation(
        input_vecobj,
        field_symbol,
        t,
        d,
        rounds_f,
        rounds_p,
        mds_vecobj,
        rc_vecobj,
    )
}

#[test]
fn test_poseidon_invalid_field_symbol() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon_invalid(
        &host,
        |_host, _input, field, _t, _d, _rounds_f, _rounds_p, _mds, _rc| {
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
fn test_poseidon_input_length_mismatch() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon_invalid(
        &host,
        |host, input, _field, _t, _d, _rounds_f, _rounds_p, _mds, _rc| {
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
fn test_poseidon_mds_dimension_mismatch() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon_invalid(
        &host,
        |host, _input, _field, t, _d, _rounds_f, _rounds_p, mds, _rc| {
            // Set t to 4 but keep mds as 3x3
            *t = U32Val::from(4);

            // Or alternatively, create a 2x2 mds matrix
            let wrong_mds: Vec<Vec<BnScalar>> = vec![
                vec![BnScalar::from(1), BnScalar::from(2)],
                vec![BnScalar::from(3), BnScalar::from(4)],
            ];
            *mds = vec_of_vec_to_vecobj(host, &wrong_mds)?;
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
fn test_poseidon_mds_not_square() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon_invalid(
        &host,
        |host, _input, _field, _t, _d, _rounds_f, _rounds_p, mds, _rc| {
            // Create a non-square mds matrix (3x2 instead of 3x3)
            let wrong_mds: Vec<Vec<BnScalar>> = vec![
                vec![BnScalar::from(1), BnScalar::from(2)],
                vec![BnScalar::from(3), BnScalar::from(4)],
                vec![BnScalar::from(5), BnScalar::from(6)],
            ];
            *mds = vec_of_vec_to_vecobj(host, &wrong_mds)?;
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
fn test_poseidon_mds_rows_different_sizes() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon_invalid(
        &host,
        |host, _input, _field, _t, _d, _rounds_f, _rounds_p, mds, _rc| {
            // Create an mds matrix with rows of different sizes
            let wrong_mds: Vec<Vec<BnScalar>> = vec![
                vec![BnScalar::from(1), BnScalar::from(2), BnScalar::from(3)],
                vec![BnScalar::from(4), BnScalar::from(5)], // Wrong size
                vec![BnScalar::from(6), BnScalar::from(7), BnScalar::from(8)],
            ];
            *mds = vec_of_vec_to_vecobj(host, &wrong_mds)?;
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
fn test_poseidon_rounds_inconsistent_with_round_constants() -> Result<(), HostError> {
    let host = Host::test_host();

    use super::poseidon_instance_bn254::POSEIDON_BN_PARAMS;
    let params = &**POSEIDON_BN_PARAMS;
    let original_rounds_f = (params.rounds_f_beginning * 2) as u32;
    let original_rounds_p = params.rounds_p as u32;

    // Test case 1: rounds_f decreased by 2
    let result = test_poseidon_invalid(
        &host,
        |_host, _input, _field, _t, _d, rounds_f, _rounds_p, _mds, _rc| {
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
    let result = test_poseidon_invalid(
        &host,
        |_host, _input, _field, _t, _d, rounds_f, _rounds_p, _mds, _rc| {
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
    let result = test_poseidon_invalid(
        &host,
        |_host, _input, _field, _t, _d, _rounds_f, rounds_p, _mds, _rc| {
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
    let result = test_poseidon_invalid(
        &host,
        |_host, _input, _field, _t, _d, _rounds_f, rounds_p, _mds, _rc| {
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
    let result = test_poseidon_invalid(
        &host,
        |_host, _input, _field, _t, _d, rounds_f, rounds_p, _mds, _rc| {
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
fn test_poseidon_invalid_degree() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon_invalid(
        &host,
        |_host, _input, _field, _t, d, _rounds_f, _rounds_p, _mds, _rc| {
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
fn test_poseidon_invalid_t() -> Result<(), HostError> {
    let host = Host::test_host();

    let result = test_poseidon_invalid(
        &host,
        |_host, _input, _field, t, _d, _rounds_f, _rounds_p, _mds, _rc| {
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
fn test_poseidon_mismatching_field_types() -> Result<(), HostError> {
    let host = Host::test_host();

    // Use BN254 parameters
    use super::poseidon_instance_bn254::POSEIDON_BN_PARAMS;
    let bn_params = &**POSEIDON_BN_PARAMS;

    // Create input that can be interpreted in both fields (small values)
    let input_bn: Vec<BnScalar> = vec![BnScalar::from(1), BnScalar::from(2), BnScalar::from(3)];
    let input_bls: Vec<BlsScalar> =
        vec![BlsScalar::from(1), BlsScalar::from(2), BlsScalar::from(3)];

    // Run Poseidon with BN254 field
    let input_vecobj_bn = host.metered_scalar_vec_to_vecobj(input_bn)?;
    let mds_vecobj = vec_of_vec_to_vecobj(&host, &bn_params.mds)?;
    let rc_vecobj = vec_of_vec_to_vecobj(&host, &bn_params.round_constants)?;

    let result_bn = host.poseidon_permutation(
        input_vecobj_bn,
        Symbol::try_from_small_str("BN254")?,
        U32Val::from(bn_params.t as u32),
        U32Val::from(bn_params.d as u32),
        U32Val::from((bn_params.rounds_f_beginning * 2) as u32),
        U32Val::from(bn_params.rounds_p as u32),
        mds_vecobj,
        rc_vecobj,
    )?;

    // Run Poseidon with BLS12_381 field using the SAME constants
    // (but interpreted as BLS12_381 field elements)
    let input_vecobj_bls = host.metered_scalar_vec_to_vecobj(input_bls)?;
    let mds_vecobj_bls = vec_of_vec_to_vecobj(&host, &bn_params.mds)?;
    let rc_vecobj_bls = vec_of_vec_to_vecobj(&host, &bn_params.round_constants)?;

    let result_bls = host.poseidon_permutation(
        input_vecobj_bls,
        Symbol::try_from_small_str("BLS12_381")?,
        U32Val::from(bn_params.t as u32),
        U32Val::from(bn_params.d as u32),
        U32Val::from((bn_params.rounds_f_beginning * 2) as u32),
        U32Val::from(bn_params.rounds_p as u32),
        mds_vecobj_bls,
        rc_vecobj_bls,
    )?;

    // The results should NOT be equal because the field modulus operations are different
    assert_ne!(
        host.obj_cmp(result_bn.into(), result_bls.into())?,
        Ordering::Equal as i64,
        "Poseidon results should differ when using different field types with same constants"
    );

    Ok(())
}

// The modules below tests reference test vectors for correctness
#[cfg(test)]
mod poseidon_tests_bls12 {
    use super::super::poseidon_instance_bls12::{
        POSEIDON_BLS_2_PARAMS, POSEIDON_BLS_3_PARAMS, POSEIDON_BLS_4_PARAMS, POSEIDON_BLS_8_PARAMS,
    };
    use super::super::poseidon_instance_hadeshash_bls12::{
        POSEIDONPERM_X5_255_3, POSEIDONPERM_X5_255_5,
    };
    use super::*;

    type Scalar = BlsScalar;

    static TESTRUNS: usize = 5;

    #[test]
    fn consistent_perm() {
        let host = Host::test_host();
        let mut rng = StdRng::from_seed([0xff; 32]);
        let instances = vec![
            Poseidon::new((**POSEIDON_BLS_2_PARAMS).clone()),
            Poseidon::new((**POSEIDON_BLS_3_PARAMS).clone()),
            Poseidon::new((**POSEIDON_BLS_4_PARAMS).clone()),
            Poseidon::new((**POSEIDON_BLS_8_PARAMS).clone()),
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
        let poseidon_2 = Poseidon::new((**POSEIDON_BLS_2_PARAMS).clone());
        let input_2: Vec<Scalar> = vec![Scalar::from(0), Scalar::from(1)];
        let perm_2 = poseidon_2.permutation(&host, &input_2).unwrap();
        assert_eq!(
            perm_2[0],
            from_hex("0x1dc37ce34aeee058292bb73bff9acffce73a8a92f3d6d1daa8b77d9516b5c837")
        );
        assert_eq!(
            perm_2[1],
            from_hex("0x534cc8001b9c21da25d62749e136ea3d702651ba129f0d5ed7847cf81bc8b042")
        );

        let poseidon_3 = Poseidon::new((**POSEIDON_BLS_3_PARAMS).clone());
        let input_3: Vec<Scalar> = vec![Scalar::from(0), Scalar::from(1), Scalar::from(2)];
        let perm_3 = poseidon_3.permutation(&host, &input_3).unwrap();
        assert_eq!(
            perm_3[0],
            from_hex("0x200e6982ac00df8fa65cef1fde9f21373fdbbfd98f2df1eb5fa04f3302ab0397")
        );
        assert_eq!(
            perm_3[1],
            from_hex("0x2233c9a40d91c1f643b700f836a1ac231c3f3a8d438ad1609355e1b7317a47e5")
        );
        assert_eq!(
            perm_3[2],
            from_hex("0x2eae6736db3c086ad29938869dedbf969dd9804a58aa228ec467b7d5a08dc765")
        );
    }

    #[test]
    fn hadeshash_x5_255_3_test_cases() {
        let host = Host::test_host();
        // poseidonperm_x5_255_3
        let poseidon = Poseidon::new((**POSEIDONPERM_X5_255_3).clone());

        // Input
        let input: Vec<Scalar> = vec![
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000000"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000001"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000002"),
        ];

        // Expected output
        let expected_output: Vec<Scalar> = vec![
            from_hex("0x28ce19420fc246a05553ad1e8c98f5c9d67166be2c18e9e4cb4b4e317dd2a78a"),
            from_hex("0x51f3e312c95343a896cfd8945ea82ba956c1118ce9b9859b6ea56637b4b1ddc4"),
            from_hex("0x3b2b69139b235626a0bfb56c9527ae66a7bf486ad8c11c14d1da0c69bbe0f79a"),
        ];

        // Run the permutation
        let result = poseidon.permutation(&host, &input).unwrap();

        // Verify the output matches expected values
        assert_eq!(result.len(), expected_output.len());
        for (i, (actual, expected)) in result.iter().zip(expected_output.iter()).enumerate() {
            assert_eq!(
                actual, expected,
                "Mismatch at index {}: got {:?}, expected {:?}",
                i, actual, expected
            );
        }
    }

    #[test]
    fn hadeshash_x5_255_5_test_cases() {
        let host = Host::test_host();
        // poseidonperm_x5_255_5
        let poseidon = Poseidon::new((**POSEIDONPERM_X5_255_5).clone());

        // Input
        let input: Vec<Scalar> = vec![
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000000"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000001"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000002"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000003"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000004"),
        ];

        // Expected output
        let expected_output: Vec<Scalar> = vec![
            from_hex("0x2a918b9c9f9bd7bb509331c81e297b5707f6fc7393dcee1b13901a0b22202e18"),
            from_hex("0x65ebf8671739eeb11fb217f2d5c5bf4a0c3f210e3f3cd3b08b5db75675d797f7"),
            from_hex("0x2cc176fc26bc70737a696a9dfd1b636ce360ee76926d182390cdb7459cf585ce"),
            from_hex("0x4dc4e29d283afd2a491fe6aef122b9a968e74eff05341f3cc23fda1781dcb566"),
            from_hex("0x03ff622da276830b9451b88b85e6184fd6ae15c8ab3ee25a5667be8592cce3b1"),
        ];

        // Run the permutation
        let result = poseidon.permutation(&host, &input).unwrap();

        // Verify the output matches expected values
        assert_eq!(result.len(), expected_output.len());
        for (i, (actual, expected)) in result.iter().zip(expected_output.iter()).enumerate() {
            assert_eq!(
                actual, expected,
                "Mismatch at index {}: got {:?}, expected {:?}",
                i, actual, expected
            );
        }
    }
}

#[cfg(test)]
mod poseidon_tests_bn254 {
    use super::super::poseidon_instance_bn254::POSEIDON_BN_PARAMS;
    use super::super::poseidon_instance_hadeshash_bn254::{
        POSEIDONPERM_X5_254_3, POSEIDONPERM_X5_254_5,
    };
    use super::*;

    type Scalar = BnScalar;

    static TESTRUNS: usize = 5;

    #[test]
    fn consistent_perm() {
        let host = Host::test_host();
        let mut rng = StdRng::from_seed([0xff; 32]);
        let poseidon = Poseidon::new((**POSEIDON_BN_PARAMS).clone());
        let t = poseidon.params.t;
        for _ in 0..TESTRUNS {
            let input1: Vec<Scalar> = (0..t).map(|_| random_scalar(&mut rng)).collect();

            let mut input2: Vec<Scalar>;
            loop {
                input2 = (0..t).map(|_| random_scalar(&mut rng)).collect();
                if input1 != input2 {
                    break;
                }
            }

            let perm1 = poseidon.permutation(&host, &input1).unwrap();
            let perm2 = poseidon.permutation(&host, &input1).unwrap();
            let perm3 = poseidon.permutation(&host, &input2).unwrap();
            assert_eq!(perm1, perm2);
            assert_ne!(perm1, perm3);
        }
    }

    #[test]
    fn kats() {
        let host = Host::test_host();
        let poseidon = Poseidon::new((**POSEIDON_BN_PARAMS).clone());
        let input: Vec<Scalar> = vec![Scalar::from(0), Scalar::from(1), Scalar::from(2)];
        let perm = poseidon.permutation(&host, &input).unwrap();
        assert_eq!(
            perm[0],
            from_hex("0x2677d68d9cfa91f197bf5148b50afac461b6b8340ff119a5217794770baade5f")
        );
        assert_eq!(
            perm[1],
            from_hex("0x21ae9d716173496b62c76ad7deb4654961f64334441bcf77e17a047155a3239f")
        );
        assert_eq!(
            perm[2],
            from_hex("0x008f8e7c73ff20b6a141c48cef73215860acc749b14f0a7887f74950215169c6")
        );
    }

    #[test]
    fn hadeshash_x5_254_3() {
        let host = Host::test_host();
        // poseidonperm_x5_254_3
        let poseidon = Poseidon::new((**POSEIDONPERM_X5_254_3).clone());

        // Input
        let input: Vec<Scalar> = vec![
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000000"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000001"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000002"),
        ];

        // Expected output
        let expected_output: Vec<Scalar> = vec![
            from_hex("0x115cc0f5e7d690413df64c6b9662e9cf2a3617f2743245519e19607a4417189a"),
            from_hex("0x0fca49b798923ab0239de1c9e7a4a9a2210312b6a2f616d18b5a87f9b628ae29"),
            from_hex("0x0e7ae82e40091e63cbd4f16a6d16310b3729d4b6e138fcf54110e2867045a30c"),
        ];

        // Run the permutation
        let result = poseidon.permutation(&host, &input).unwrap();

        // Verify the output matches expected values
        assert_eq!(result.len(), expected_output.len());
        for (i, (actual, expected)) in result.iter().zip(expected_output.iter()).enumerate() {
            assert_eq!(
                actual, expected,
                "Mismatch at index {}: got {:?}, expected {:?}",
                i, actual, expected
            );
        }
    }

    #[test]
    fn hadeshash_x5_254_5() {
        let host = Host::test_host();
        // poseidonperm_x5_254_5
        let poseidon = Poseidon::new((**POSEIDONPERM_X5_254_5).clone());

        // Input
        let input: Vec<Scalar> = vec![
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000000"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000001"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000002"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000003"),
            from_hex("0x0000000000000000000000000000000000000000000000000000000000000004"),
        ];

        // Expected output
        let expected_output: Vec<Scalar> = vec![
            from_hex("0x299c867db6c1fdd79dcefa40e4510b9837e60ebb1ce0663dbaa525df65250465"),
            from_hex("0x1148aaef609aa338b27dafd89bb98862d8bb2b429aceac47d86206154ffe053d"),
            from_hex("0x24febb87fed7462e23f6665ff9a0111f4044c38ee1672c1ac6b0637d34f24907"),
            from_hex("0x0eb08f6d809668a981c186beaf6110060707059576406b248e5d9cf6e78b3d3e"),
            from_hex("0x07748bc6877c9b82c8b98666ee9d0626ec7f5be4205f79ee8528ef1c4a376fc7"),
        ];

        // Run the permutation
        let result = poseidon.permutation(&host, &input).unwrap();

        // Verify the output matches expected values
        assert_eq!(result.len(), expected_output.len());
        for (i, (actual, expected)) in result.iter().zip(expected_output.iter()).enumerate() {
            assert_eq!(
                actual, expected,
                "Mismatch at index {}: got {:?}, expected {:?}",
                i, actual, expected
            );
        }
    }
}
