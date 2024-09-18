use crate::common::HostCostMeasurement;
use ark_bls12_381::{Fq, Fq2, Fr, G1Affine, G1Projective, G2Affine, G2Projective};
use ark_ff::UniformRand;
use ark_serialize::CanonicalSerialize;
use rand::{rngs::StdRng, Rng, RngCore};
use soroban_env_host::{
    cost_runner::{
        Bls12381DecodeFpRun, Bls12381DecodeFpSample, Bls12381EncodeFpRun, Bls12381EncodeFpSample,
        Bls12381FrAddRun, Bls12381FrAddSubMulSample, Bls12381FrFromU256Run,
        Bls12381FrFromU256Sample, Bls12381FrInvRun, Bls12381FrInvSample, Bls12381FrMulRun,
        Bls12381FrPowRun, Bls12381FrPowSample, Bls12381FrSubRun, Bls12381FrToU256Run,
        Bls12381FrToU256Sample, Bls12381G1AddRun, Bls12381G1AddSample,
        Bls12381G1CheckPointInSubgroupRun, Bls12381G1CheckPointInSubgroupSample,
        Bls12381G1CheckPointOnCurveRun, Bls12381G1CheckPointOnCurveSample, Bls12381G1MsmRun,
        Bls12381G1MsmSample, Bls12381G1MulRun, Bls12381G1MulSample,
        Bls12381G1ProjectiveToAffineRun, Bls12381G1ProjectiveToAffineSample, Bls12381G2AddRun,
        Bls12381G2AddSample, Bls12381G2CheckPointInSubgroupRun,
        Bls12381G2CheckPointInSubgroupSample, Bls12381G2CheckPointOnCurveRun,
        Bls12381G2CheckPointOnCurveSample, Bls12381G2MsmRun, Bls12381G2MsmSample, Bls12381G2MulRun,
        Bls12381G2MulSample, Bls12381G2ProjectiveToAffineRun, Bls12381G2ProjectiveToAffineSample,
        Bls12381HashToG1Run, Bls12381HashToG1Sample, Bls12381HashToG2Run, Bls12381HashToG2Sample,
        Bls12381MapFp2ToG2Run, Bls12381MapFp2ToG2Sample, Bls12381MapFpToG1Run,
        Bls12381MapFpToG1Sample, Bls12381PairingRun, Bls12381PairingSample,
    },
    xdr::ContractCostType::*,
    Host, TryIntoVal, U256Val, U256,
};

pub(crate) struct Bls12381EncodeFpMeasure;
impl HostCostMeasurement for Bls12381EncodeFpMeasure {
    type Runner = Bls12381EncodeFpRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381EncodeFpSample {
        let buf = vec![0; 1000];
        let fp = Fq::rand(rng);
        Bls12381EncodeFpSample(buf, fp)
    }
}
pub(crate) struct Bls12381DecodeFpMeasure;
impl HostCostMeasurement for Bls12381DecodeFpMeasure {
    type Runner = Bls12381DecodeFpRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381DecodeFpSample {
        let mut buf = vec![];
        let _ = Fq::rand(rng).serialize_uncompressed(&mut buf).unwrap();
        Bls12381DecodeFpSample(buf)
    }
}
pub(crate) struct Bls12381G1CheckPointOnCurveMeasure;
impl HostCostMeasurement for Bls12381G1CheckPointOnCurveMeasure {
    type Runner = Bls12381G1CheckPointOnCurveRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> Bls12381G1CheckPointOnCurveSample {
        Bls12381G1CheckPointOnCurveSample(
            G1Affine::new_unchecked(Fq::rand(rng), Fq::rand(rng)),
            Bls12381G1CheckPointOnCurve,
        )
    }
}
pub(crate) struct Bls12381G1CheckPointInSubgroupMeasure;
impl HostCostMeasurement for Bls12381G1CheckPointInSubgroupMeasure {
    type Runner = Bls12381G1CheckPointInSubgroupRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> Bls12381G1CheckPointInSubgroupSample {
        Bls12381G1CheckPointInSubgroupSample(G1Affine::rand(rng), Bls12381G1CheckPointInSubgroup)
    }
}
pub(crate) struct Bls12381G2CheckPointOnCurveMeasure;
impl HostCostMeasurement for Bls12381G2CheckPointOnCurveMeasure {
    type Runner = Bls12381G2CheckPointOnCurveRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> Bls12381G2CheckPointOnCurveSample {
        Bls12381G2CheckPointOnCurveSample(
            G2Affine::new_unchecked(Fq2::rand(rng), Fq2::rand(rng)),
            Bls12381G2CheckPointOnCurve,
        )
    }
}
pub(crate) struct Bls12381G2CheckPointInSubgroupMeasure;
impl HostCostMeasurement for Bls12381G2CheckPointInSubgroupMeasure {
    type Runner = Bls12381G2CheckPointInSubgroupRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> Bls12381G2CheckPointInSubgroupSample {
        Bls12381G2CheckPointInSubgroupSample(G2Affine::rand(rng), Bls12381G2CheckPointInSubgroup)
    }
}
pub(crate) struct Bls12381FrFromU256Measure;
impl HostCostMeasurement for Bls12381FrFromU256Measure {
    type Runner = Bls12381FrFromU256Run;

    fn new_random_case(host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381FrFromU256Sample {
        let mut buf = [0; 32];
        rng.fill_bytes(&mut buf);
        let u = U256::from_be_bytes(buf);
        let val: U256Val = u.try_into_val(host).unwrap();
        Bls12381FrFromU256Sample(val)
    }
}
pub(crate) struct Bls12381FrToU256Measure;
impl HostCostMeasurement for Bls12381FrToU256Measure {
    type Runner = Bls12381FrToU256Run;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381FrToU256Sample {
        Bls12381FrToU256Sample(Fr::rand(rng))
    }
}
pub(crate) struct Bls12381FrAddMeasure;
impl HostCostMeasurement for Bls12381FrAddMeasure {
    type Runner = Bls12381FrAddRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381FrAddSubMulSample {
        Bls12381FrAddSubMulSample(Fr::rand(rng), Fr::rand(rng))
    }
}
pub(crate) struct Bls12381FrSubMeasure;
impl HostCostMeasurement for Bls12381FrSubMeasure {
    type Runner = Bls12381FrSubRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381FrAddSubMulSample {
        Bls12381FrAddSubMulSample(Fr::rand(rng), Fr::rand(rng))
    }
}
pub(crate) struct Bls12381FrMulMeasure;
impl HostCostMeasurement for Bls12381FrMulMeasure {
    type Runner = Bls12381FrMulRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381FrAddSubMulSample {
        Bls12381FrAddSubMulSample(Fr::rand(rng), Fr::rand(rng))
    }
}
pub(crate) struct Bls12381FrPowMeasure;
impl HostCostMeasurement for Bls12381FrPowMeasure {
    type Runner = Bls12381FrPowRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Bls12381FrPowSample {
        assert!(input <= 64);
        let rhs = if input == 64 {
            u64::MAX
        } else {
            (1 << input) - 1
        };
        Bls12381FrPowSample(Fr::rand(rng), rhs)
    }
}
pub(crate) struct Bls12381FrInvMeasure;
impl HostCostMeasurement for Bls12381FrInvMeasure {
    type Runner = Bls12381FrInvRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381FrInvSample {
        Bls12381FrInvSample(Fr::rand(rng))
    }
}

pub(crate) struct Bls12381G1AddMeasure;

impl HostCostMeasurement for Bls12381G1AddMeasure {
    type Runner = Bls12381G1AddRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381G1AddSample {
        let p0 = G1Affine::rand(rng);
        let p1 = G1Affine::rand(rng);
        Bls12381G1AddSample(p0, p1)
    }
}

pub(crate) struct Bls12381G1ProjectiveToAffineMeasure;

impl HostCostMeasurement for Bls12381G1ProjectiveToAffineMeasure {
    type Runner = Bls12381G1ProjectiveToAffineRun;

    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> Bls12381G1ProjectiveToAffineSample {
        let p0 = G1Projective::rand(rng);
        Bls12381G1ProjectiveToAffineSample(p0)
    }
}

pub(crate) struct Bls12381G1MulMeasure;

impl HostCostMeasurement for Bls12381G1MulMeasure {
    type Runner = Bls12381G1MulRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381G1MulSample {
        let p = G1Affine::rand(rng);
        let s = Fr::rand(rng);
        Bls12381G1MulSample(p, s)
    }
}

pub(crate) struct Bls12381G1MsmMeasure;

impl HostCostMeasurement for Bls12381G1MsmMeasure {
    type Runner = Bls12381G1MsmRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Bls12381G1MsmSample {
        Bls12381G1MsmSample(
            (0..input)
                .into_iter()
                .map(|_| G1Affine::rand(rng))
                .collect(),
            (0..input).into_iter().map(|_| Fr::rand(rng)).collect(),
            Bls12381G1Msm,
            "G1".to_string(),
        )
    }
}

pub(crate) struct Bls12381MapFpToG1Measure;

impl HostCostMeasurement for Bls12381MapFpToG1Measure {
    type Runner = Bls12381MapFpToG1Run;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381MapFpToG1Sample {
        let fp = Fq::rand(rng);
        Bls12381MapFpToG1Sample(fp, Bls12381MapFpToG1)
    }
}

pub(crate) struct Bls12381HashToG1Measure;

impl HostCostMeasurement for Bls12381HashToG1Measure {
    type Runner = Bls12381HashToG1Run;
    const STEP_SIZE: u64 = 64;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Bls12381HashToG1Sample {
        let len = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        let domain = "SOROBAN-V01-CS01-with-BLS12381G1_XMD:SHA-256_SSWU_RO_"
            .as_bytes()
            .to_vec();
        let mut msg = vec![0u8; len as usize];
        rng.fill(msg.as_mut_slice());
        Bls12381HashToG1Sample(domain, msg, Bls12381HashToG1)
    }
}

pub(crate) struct Bls12381G2ProjectiveToAffineMeasure;

impl HostCostMeasurement for Bls12381G2ProjectiveToAffineMeasure {
    type Runner = Bls12381G2ProjectiveToAffineRun;

    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> Bls12381G2ProjectiveToAffineSample {
        let p0 = G2Projective::rand(rng);
        Bls12381G2ProjectiveToAffineSample(p0)
    }
}

pub(crate) struct Bls12381G2AddMeasure;

impl HostCostMeasurement for Bls12381G2AddMeasure {
    type Runner = Bls12381G2AddRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381G2AddSample {
        let p0 = G2Affine::rand(rng);
        let p1 = G2Affine::rand(rng);
        Bls12381G2AddSample(p0, p1)
    }
}

pub(crate) struct Bls12381G2MulMeasure;

impl HostCostMeasurement for Bls12381G2MulMeasure {
    type Runner = Bls12381G2MulRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381G2MulSample {
        let p = G2Affine::rand(rng);
        let s = Fr::rand(rng);
        Bls12381G2MulSample(p, s)
    }
}

pub(crate) struct Bls12381G2MsmMeasure;

impl HostCostMeasurement for Bls12381G2MsmMeasure {
    type Runner = Bls12381G2MsmRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Bls12381G2MsmSample {
        Bls12381G2MsmSample(
            (0..input)
                .into_iter()
                .map(|_| G2Affine::rand(rng))
                .collect(),
            (0..input).into_iter().map(|_| Fr::rand(rng)).collect(),
            Bls12381G2Msm,
            "G2".to_string(),
        )
    }
}

pub(crate) struct Bls12381MapFp2ToG2Measure;

impl HostCostMeasurement for Bls12381MapFp2ToG2Measure {
    type Runner = Bls12381MapFp2ToG2Run;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bls12381MapFp2ToG2Sample {
        let fp2 = Fq2::rand(rng);
        Bls12381MapFp2ToG2Sample(fp2, Bls12381MapFp2ToG2)
    }
}

pub(crate) struct Bls12381HashToG2Measure;

impl HostCostMeasurement for Bls12381HashToG2Measure {
    type Runner = Bls12381HashToG2Run;
    const STEP_SIZE: u64 = 64;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Bls12381HashToG2Sample {
        let len = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        let domain = "SOROBAN-V01-CS01-with-BLS12381G2_XMD:SHA-256_SSWU_RO_"
            .as_bytes()
            .to_vec();
        let mut msg = vec![0u8; len as usize];
        rng.fill(msg.as_mut_slice());
        Bls12381HashToG2Sample(domain, msg, Bls12381HashToG2)
    }
}

pub(crate) struct Bls12381PairingMeasure;

impl HostCostMeasurement for Bls12381PairingMeasure {
    type Runner = Bls12381PairingRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Bls12381PairingSample {
        Bls12381PairingSample(
            (0..input)
                .into_iter()
                .map(|_| G1Affine::rand(rng))
                .collect(),
            (0..input)
                .into_iter()
                .map(|_| G2Affine::rand(rng))
                .collect(),
        )
    }
}
