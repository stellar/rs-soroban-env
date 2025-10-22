use crate::{
    crypto::poseidon::{
        poseidon2::Poseidon2,
    },
    testutils::crypto::{from_hex, random_scalar},
    Host,
};
use ark_bls12_381::Fr as BlsScalar;
use ark_bn254::Fr as BnScalar;
use rand::{rngs::StdRng, SeedableRng};

use super::poseidon2_instance_bls12::{
    POSEIDON2_BLS_2_PARAMS,
    POSEIDON2_BLS_3_PARAMS,
    POSEIDON2_BLS_4_PARAMS,
    POSEIDON2_BLS_8_PARAMS,
};

#[cfg(test)]
mod poseidon2_tests_bls12 {
    use super::*;

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
            Poseidon2::new((**POSEIDON2_BLS_8_PARAMS).clone())
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
        assert_eq!(perm_2[0], from_hex("0x73c46dd530e248a87b61d19e67fa1b4ed30fc3d09f16531fe189fb945a15ce4e"));
        assert_eq!(perm_2[1], from_hex("0x1f0e305ee21c9366d5793b80251405032a3fee32b9dd0b5f4578262891b043b4"));

        let poseidon2_3 = Poseidon2::new((**POSEIDON2_BLS_3_PARAMS).clone());
        let mut input_3: Vec<Scalar> = vec![];
        for i in 0..poseidon2_3.params.t {
            input_3.push(Scalar::from(i as u64));
        }
        let perm_3 = poseidon2_3.permutation(&host, &input_3).unwrap();
        assert_eq!(perm_3[0], from_hex("0x1b152349b1950b6a8ca75ee4407b6e26ca5cca5650534e56ef3fd45761fbf5f0"));
        assert_eq!(perm_3[1], from_hex("0x4c5793c87d51bdc2c08a32108437dc0000bd0275868f09ebc5f36919af5b3891"));
        assert_eq!(perm_3[2], from_hex("0x1fc8ed171e67902ca49863159fe5ba6325318843d13976143b8125f08b50dc6b"));

        let poseidon2_4 = Poseidon2::new((**POSEIDON2_BLS_4_PARAMS).clone());
        let mut input_4: Vec<Scalar> = vec![];
        for i in 0..poseidon2_4.params.t {
            input_4.push(Scalar::from(i as u64));
        }
        let perm_4 = poseidon2_4.permutation(&host, &input_4).unwrap();
        assert_eq!(perm_4[0], from_hex("0x28ff6c4edf9768c08ae26290487e93449cc8bc155fc2fad92a344adceb3ada6d"));
        assert_eq!(perm_4[1], from_hex("0x0e56f2b6fad25075aa93560185b70e2b180ed7e269159c507c288b6747a0db2d"));
        assert_eq!(perm_4[2], from_hex("0x6d8196f28da6006bb89b3df94600acdc03d0ba7c2b0f3f4409a54c1db6bf30d0"));
        assert_eq!(perm_4[3], from_hex("0x07cfb49540ee456cce38b8a7d1a930a57ffc6660737f6589ef184c5e15334e36"));
    }
}

#[cfg(test)]
mod poseidon2_tests_bn254 {
    use super::*;
    use super::super::poseidon2_instance_bn254::POSEIDON2_BN254_PARAMS;

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
        assert_eq!(perm[0], from_hex("0x0bb61d24daca55eebcb1929a82650f328134334da98ea4f847f760054f4a3033"));
        assert_eq!(perm[1], from_hex("0x303b6f7c86d043bfcbcc80214f26a30277a15d3f74ca654992defe7ff8d03570"));
        assert_eq!(perm[2], from_hex("0x1ed25194542b12eef8617361c3ba7c52e660b145994427cc86296242cf766ec8"));
    }
}
