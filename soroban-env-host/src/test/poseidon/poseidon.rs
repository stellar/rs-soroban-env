use crate::{crypto::poseidon::poseidon::Poseidon, Host};
use ark_bls12_381::Fr as BlsScalar;
use ark_bn254::Fr as BnScalar;
use rand::{rngs::StdRng, SeedableRng};

use super::poseidon_instance_bls12::{
    POSEIDON_BLS_2_PARAMS, POSEIDON_BLS_3_PARAMS, POSEIDON_BLS_4_PARAMS, POSEIDON_BLS_8_PARAMS,
};

#[cfg(test)]
mod poseidon_tests_bls12 {
    use super::super::poseidon_instance_hadeshash_bls12::{
        POSEIDONPERM_X5_255_3, POSEIDONPERM_X5_255_5,
    };
    use super::*;
    use crate::testutils::crypto::{from_hex, random_scalar};

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
    use crate::testutils::crypto::{from_hex, random_scalar};

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
