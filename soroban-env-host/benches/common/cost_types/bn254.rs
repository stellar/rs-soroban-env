use crate::common::HostCostMeasurement;
use ark_bn254::{Fq, Fq2, Fr, G1Affine, G1Projective, G2Affine};
use ark_ff::UniformRand;
use ark_serialize::CanonicalSerialize;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{
    cost_runner::{
        Bn254DecodeFpRun, Bn254DecodeFpSample, Bn254EncodeFpRun, Bn254EncodeFpSample,
        Bn254FrFromU256Run, Bn254FrFromU256Sample, Bn254G1AddRun, Bn254G1AddSample,
        Bn254G1CheckPointOnCurveRun, Bn254G1CheckPointOnCurveSample, Bn254G1MulRun,
        Bn254G1MulSample, Bn254G1ProjectiveToAffineRun, Bn254G1ProjectiveToAffineSample,
        Bn254G2CheckPointInSubgroupRun, Bn254G2CheckPointInSubgroupSample,
        Bn254G2CheckPointOnCurveRun, Bn254G2CheckPointOnCurveSample, Bn254PairingRun,
        Bn254PairingSample,
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
        let p0 = G1Projective::rand(rng);
        Bn254G1ProjectiveToAffineSample(p0)
    }
}

pub(crate) struct Bn254G1MulMeasure;

impl HostCostMeasurement for Bn254G1MulMeasure {
    type Runner = Bn254G1MulRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Bn254G1MulSample {
        let p = G1Affine::rand(rng);
        let s = Fr::rand(rng);
        Bn254G1MulSample(p, s)
    }
}

pub(crate) struct Bn254PairingMeasure;

impl HostCostMeasurement for Bn254PairingMeasure {
    type Runner = Bn254PairingRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Bn254PairingSample {
        let i = input.max(2);
        Bn254PairingSample(
            (1..i).into_iter().map(|_| G1Affine::rand(rng)).collect(),
            (1..i).into_iter().map(|_| G2Affine::rand(rng)).collect(),
        )
    }
}
