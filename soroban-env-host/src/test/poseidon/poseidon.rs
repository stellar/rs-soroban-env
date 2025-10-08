use crate::{
    crypto::poseidon::{
        poseidon::Poseidon,
    },
    Host,
};
use ark_bls12_381::Fr as BlsScalar;
use ark_bn254::Fr as BnScalar;
use rand::{rngs::StdRng, SeedableRng};

use super::poseidon_instance_bls12::{
    POSEIDON_BLS_2_PARAMS,
    POSEIDON_BLS_3_PARAMS,
    POSEIDON_BLS_4_PARAMS,
    POSEIDON_BLS_8_PARAMS,
};


#[cfg(test)]
mod poseidon_tests_bls12 {
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
            Poseidon::new((**POSEIDON_BLS_8_PARAMS).clone())
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

                let perm1 = instance.permutation(&input1, &host).unwrap();
                let perm2 = instance.permutation(&input1, &host).unwrap();
                let perm3 = instance.permutation(&input2, &host).unwrap();
                assert_eq!(perm1, perm2);
                assert_ne!(perm1, perm3);
            }
        }
    }

    #[test]
    fn kats() {
        let host = Host::test_host();
        let poseidon_2 = Poseidon::new((**POSEIDON_BLS_2_PARAMS).clone());
        let input_2: Vec<Scalar> = vec![Scalar::from(0), Scalar::from(1),];
        let perm_2 = poseidon_2.permutation(&input_2, &host).unwrap();
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
        let perm_3 = poseidon_3.permutation(&input_3, &host).unwrap();
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
}

#[cfg(test)]
mod poseidon_tests_bn256 {
    use super::*;
    use crate::testutils::crypto::{from_hex, random_scalar};
    use super::super::poseidon_instance_bn256::POSEIDON_BN_PARAMS;

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

            let perm1 = poseidon.permutation(&input1, &host).unwrap();
            let perm2 = poseidon.permutation(&input1, &host).unwrap();
            let perm3 = poseidon.permutation(&input2, &host).unwrap();
            assert_eq!(perm1, perm2);
            assert_ne!(perm1, perm3);
        }
    }

    #[test]
    fn kats() {
        let host = Host::test_host();
        let poseidon = Poseidon::new((**POSEIDON_BN_PARAMS).clone());
        let input: Vec<Scalar> = vec![Scalar::from(0), Scalar::from(1), Scalar::from(2)];
        let perm = poseidon.permutation(&input, &host).unwrap();
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
}
