use crate::common::HostCostMeasurement;
use ark_bn254::{Fq, Fq2, Fr, G1Affine, G1Projective, G2Affine};
use ark_ec::AffineRepr;
use ark_ff::{BigInt, Field, One, PrimeField, UniformRand, Zero};
use ark_serialize::CanonicalSerialize;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{
    cost_runner::{
        Bn254DecodeFpRun, Bn254DecodeFpSample, Bn254EncodeFpRun, Bn254EncodeFpSample,
        Bn254FrAddRun, Bn254FrAddSubMulSample, Bn254FrFromU256Run, Bn254FrFromU256Sample,
        Bn254FrInvRun, Bn254FrInvSample, Bn254FrMulRun, Bn254FrPowRun, Bn254FrPowSample,
        Bn254FrSubRun, Bn254FrToU256Run, Bn254FrToU256Sample, Bn254G1AddRun, Bn254G1AddSample,
        Bn254G1CheckPointOnCurveRun, Bn254G1CheckPointOnCurveSample, Bn254G1MsmRun,
        Bn254G1MsmSample, Bn254G1MulRun, Bn254G1MulSample, Bn254G1ProjectiveToAffineRun,
        Bn254G1ProjectiveToAffineSample, Bn254G2CheckPointInSubgroupRun,
        Bn254G2CheckPointInSubgroupSample, Bn254G2CheckPointOnCurveRun,
        Bn254G2CheckPointOnCurveSample, Bn254PairingRun, Bn254PairingSample,
    },
    xdr::ContractCostType::*,
    Host, TryIntoVal, U256Val, U256,
};

pub(crate) struct Bn254EncodeFpMeasure;
impl HostCostMeasurement for Bn254EncodeFpMeasure {
    type Runner = Bn254EncodeFpRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bn254EncodeFpSample {
        let buf = vec![0; 32];
        let fp = Fq::rand(rng);
        Bn254EncodeFpSample(buf, fp)
    }
}

pub(crate) struct Bn254DecodeFpMeasure;
impl HostCostMeasurement for Bn254DecodeFpMeasure {
    type Runner = Bn254DecodeFpRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bn254DecodeFpSample {
        let mut buf = vec![];
        let _ = Fq::rand(rng).serialize_uncompressed(&mut buf).unwrap();
        Bn254DecodeFpSample(buf)
    }
}

pub(crate) struct Bn254G1CheckPointOnCurveMeasure;
impl HostCostMeasurement for Bn254G1CheckPointOnCurveMeasure {
    type Runner = Bn254G1CheckPointOnCurveRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> Bn254G1CheckPointOnCurveSample {
        Bn254G1CheckPointOnCurveSample(
            G1Affine::new_unchecked(Fq::rand(rng), Fq::rand(rng)),
            Bn254G1CheckPointOnCurve,
        )
    }
}

pub(crate) struct Bn254G2CheckPointOnCurveMeasure;
impl HostCostMeasurement for Bn254G2CheckPointOnCurveMeasure {
    type Runner = Bn254G2CheckPointOnCurveRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> Bn254G2CheckPointOnCurveSample {
        Bn254G2CheckPointOnCurveSample(
            G2Affine::new_unchecked(Fq2::rand(rng), Fq2::rand(rng)),
            Bn254G2CheckPointOnCurve,
        )
    }
}

pub(crate) struct Bn254G2CheckPointInSubgroupMeasure;
impl HostCostMeasurement for Bn254G2CheckPointInSubgroupMeasure {
    type Runner = Bn254G2CheckPointInSubgroupRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> Bn254G2CheckPointInSubgroupSample {
        Bn254G2CheckPointInSubgroupSample(G2Affine::rand(rng))
    }
}

pub(crate) struct Bn254FrFromU256Measure;
impl HostCostMeasurement for Bn254FrFromU256Measure {
    type Runner = Bn254FrFromU256Run;

    fn new_random_case(host: &Host, rng: &mut StdRng, _input: u64) -> Bn254FrFromU256Sample {
        let mut buf = [0; 32];
        rng.fill_bytes(&mut buf);
        let u = U256::from_be_bytes(buf);
        let val: U256Val = u.try_into_val(host).unwrap();
        Bn254FrFromU256Sample(val)
    }
}

pub(crate) struct Bn254G1AddMeasure;

impl HostCostMeasurement for Bn254G1AddMeasure {
    type Runner = Bn254G1AddRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bn254G1AddSample {
        let p0 = G1Affine::rand(rng);
        let p1 = G1Affine::rand(rng);
        Bn254G1AddSample(p0, p1)
    }
}

pub(crate) struct Bn254G1ProjectiveToAffineMeasure;

impl HostCostMeasurement for Bn254G1ProjectiveToAffineMeasure {
    type Runner = Bn254G1ProjectiveToAffineRun;

    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> Bn254G1ProjectiveToAffineSample {
        Bn254G1ProjectiveToAffineSample(random_g1_projective_with_nontrivial_z(rng))
    }
}

// A random G1 point in Jacobian form with Z not in {0, 1}, so that
// `into_affine` takes the field-inversion path. `G1Projective::rand` always
// produces Z == 1 for BN254 (its cofactor is 1), which `into_affine`
// short-circuits into a plain copy. Every host path that converts a point
// (outputs of add/mul/msm) produces Z != 1 in general.
fn random_g1_projective_with_nontrivial_z(rng: &mut StdRng) -> G1Projective {
    loop {
        let p = G1Affine::rand(rng);
        let l = Fq::rand(rng);
        if p.is_zero() || l.is_zero() || l.is_one() {
            continue;
        }
        let (x, y) = p.xy().unwrap();
        let l2 = l.square();
        let q = G1Projective::new_unchecked(x * l2, y * (l2 * l), l);
        assert!(!q.z.is_one());
        return q;
    }
}

// The canonical scalar (< r) with the maximal Hamming weight at full bit
// length: weight 253 over 254 bits. `G1Affine * Fr` uses double-and-add, which
// performs one point addition per set bit, so this maximizes the cost.
fn max_hamming_weight_fr() -> Fr {
    let s = Fr::from_bigint(BigInt([
        u64::MAX,
        u64::MAX,
        u64::MAX,
        0x2fff_ffff_ffff_ffff,
    ]))
    .unwrap();
    assert_eq!(
        s.into_bigint()
            .0
            .iter()
            .map(|l| l.count_ones())
            .sum::<u32>(),
        253
    );
    s
}

pub(crate) struct Bn254G1MulMeasure;

impl HostCostMeasurement for Bn254G1MulMeasure {
    type Runner = Bn254G1MulRun;

    fn new_worst_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bn254G1MulSample {
        Bn254G1MulSample(G1Affine::rand(rng), max_hamming_weight_fr())
    }

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bn254G1MulSample {
        let p = G1Affine::rand(rng);
        let s = Fr::rand(rng);
        Bn254G1MulSample(p, s)
    }
}

pub(crate) struct Bn254G1MsmMeasure;

impl HostCostMeasurement for Bn254G1MsmMeasure {
    type Runner = Bn254G1MsmRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Bn254G1MsmSample {
        Bn254G1MsmSample(
            (0..input)
                .into_iter()
                .map(|_| G1Affine::rand(rng))
                .collect(),
            (0..input).into_iter().map(|_| Fr::rand(rng)).collect(),
            Bn254G1Msm,
            "G1".to_string(),
        )
    }
}

pub(crate) struct Bn254PairingMeasure;

impl HostCostMeasurement for Bn254PairingMeasure {
    type Runner = Bn254PairingRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Bn254PairingSample {
        let i = input.max(1);
        Bn254PairingSample(
            (0..i).into_iter().map(|_| G1Affine::rand(rng)).collect(),
            (0..i).into_iter().map(|_| G2Affine::rand(rng)).collect(),
        )
    }
}

pub(crate) struct Bn254FrToU256Measure;
impl HostCostMeasurement for Bn254FrToU256Measure {
    type Runner = Bn254FrToU256Run;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bn254FrToU256Sample {
        Bn254FrToU256Sample(Fr::rand(rng))
    }
}

pub(crate) struct Bn254FrAddMeasure;
impl HostCostMeasurement for Bn254FrAddMeasure {
    type Runner = Bn254FrAddRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bn254FrAddSubMulSample {
        Bn254FrAddSubMulSample(Fr::rand(rng), Fr::rand(rng))
    }
}

pub(crate) struct Bn254FrSubMeasure;
impl HostCostMeasurement for Bn254FrSubMeasure {
    type Runner = Bn254FrSubRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bn254FrAddSubMulSample {
        Bn254FrAddSubMulSample(Fr::rand(rng), Fr::rand(rng))
    }
}

pub(crate) struct Bn254FrMulMeasure;
impl HostCostMeasurement for Bn254FrMulMeasure {
    type Runner = Bn254FrMulRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bn254FrAddSubMulSample {
        Bn254FrAddSubMulSample(Fr::rand(rng), Fr::rand(rng))
    }
}

pub(crate) struct Bn254FrPowMeasure;
impl HostCostMeasurement for Bn254FrPowMeasure {
    type Runner = Bn254FrPowRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Bn254FrPowSample {
        assert!(input <= 64);
        let rhs = if input == 64 {
            u64::MAX
        } else {
            (1 << input) - 1
        };
        Bn254FrPowSample(Fr::rand(rng), rhs)
    }
}

pub(crate) struct Bn254FrInvMeasure;
impl HostCostMeasurement for Bn254FrInvMeasure {
    type Runner = Bn254FrInvRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bn254FrInvSample {
        Bn254FrInvSample(Fr::rand(rng))
    }
}
