use crate::common::HostCostMeasurement;
use ark_bls12_381::{Fq, Fq2, G1Affine, G2Affine};
use ark_ff::UniformRand;
use ark_serialize::CanonicalSerialize;
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::{
        Bls12381Fp2DeserializeUncompressedRun, Bls12381G1AffineDeserializeUncompressedRun,
        Bls12381G1AffineSerializeUncompressedRun, Bls12381G1CheckPointInSubgroupRun,
        Bls12381G1CheckPointInSubgroupSample, Bls12381G1CheckPointOnCurveRun,
        Bls12381G1CheckPointOnCurveSample, Bls12381G1ComputeYFromXRun,
        Bls12381G1ComputeYFromXSample, Bls12381G2AffineDeserializeUncompressedRun,
        Bls12381G2AffineSerializeUncompressedRun, Bls12381G2CheckPointInSubgroupRun,
        Bls12381G2CheckPointInSubgroupSample, Bls12381G2CheckPointOnCurveRun,
        Bls12381G2CheckPointOnCurveSample, Bls12381G2ComputeYFromXRun,
        Bls12381G2ComputeYFromXSample, CostRunner,
    },
    Host,
};

pub(crate) struct Bls12381G1AffineSerializeUncompressedMeasure;
impl HostCostMeasurement for Bls12381G1AffineSerializeUncompressedMeasure {
    type Runner = Bls12381G1AffineSerializeUncompressedRun;
    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> G1Affine {
        G1Affine::rand(rng)
    }
}
pub(crate) struct Bls12381G2AffineSerializeUncompressedMeasure;
impl HostCostMeasurement for Bls12381G2AffineSerializeUncompressedMeasure {
    type Runner = Bls12381G2AffineSerializeUncompressedRun;
    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> G2Affine {
        G2Affine::rand(rng)
    }
}

pub(crate) struct Bls12381G1AffineDeserializeUncompressedMeasure;
impl HostCostMeasurement for Bls12381G1AffineDeserializeUncompressedMeasure {
    type Runner = Bls12381G1AffineDeserializeUncompressedRun;
    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Vec<u8> {
        let mut buf = vec![];
        let _ = G1Affine::rand(rng)
            .serialize_uncompressed(&mut buf)
            .unwrap();
        buf
    }
}
pub(crate) struct Bls12381G2AffineDeserializeUncompressedMeasure;
impl HostCostMeasurement for Bls12381G2AffineDeserializeUncompressedMeasure {
    type Runner = Bls12381G2AffineDeserializeUncompressedRun;
    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Vec<u8> {
        let mut buf = vec![];
        let _ = G2Affine::rand(rng)
            .serialize_uncompressed(&mut buf)
            .unwrap();
        buf
    }
}

pub(crate) struct Bls12381Fp2DeserializeUncompressedMeasure;
impl HostCostMeasurement for Bls12381Fp2DeserializeUncompressedMeasure {
    type Runner = Bls12381Fp2DeserializeUncompressedRun;
    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Vec<u8> {
        let mut buf = vec![];
        let _ = Fq2::rand(rng).serialize_uncompressed(&mut buf).unwrap();
        buf
    }
}

pub(crate) struct Bls12381G1CheckPointOnCurveMeasure;
impl HostCostMeasurement for Bls12381G1CheckPointOnCurveMeasure {
    type Runner = Bls12381G1CheckPointOnCurveRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        Bls12381G1CheckPointOnCurveSample(G1Affine::new_unchecked(Fq::rand(rng), Fq::rand(rng)))
    }
}
pub(crate) struct Bls12381G1CheckPointInSubgroupMeasure;
impl HostCostMeasurement for Bls12381G1CheckPointInSubgroupMeasure {
    type Runner = Bls12381G1CheckPointInSubgroupRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        Bls12381G1CheckPointInSubgroupSample(G1Affine::rand(rng))
    }
}
pub(crate) struct Bls12381G1ComputeYFromXMeasure;
impl HostCostMeasurement for Bls12381G1ComputeYFromXMeasure {
    type Runner = Bls12381G1ComputeYFromXRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        Bls12381G1ComputeYFromXSample(G1Affine::rand(rng))
    }
}
pub(crate) struct Bls12381G2CheckPointOnCurveMeasure;
impl HostCostMeasurement for Bls12381G2CheckPointOnCurveMeasure {
    type Runner = Bls12381G2CheckPointOnCurveRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        Bls12381G2CheckPointOnCurveSample(G2Affine::new_unchecked(Fq2::rand(rng), Fq2::rand(rng)))
    }
}
pub(crate) struct Bls12381G2CheckPointInSubgroupMeasure;
impl HostCostMeasurement for Bls12381G2CheckPointInSubgroupMeasure {
    type Runner = Bls12381G2CheckPointInSubgroupRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        Bls12381G2CheckPointInSubgroupSample(G2Affine::rand(rng))
    }
}
pub(crate) struct Bls12381G2ComputeYFromXMeasure;
impl HostCostMeasurement for Bls12381G2ComputeYFromXMeasure {
    type Runner = Bls12381G2ComputeYFromXRun;
    fn new_random_case(
        _host: &Host,
        rng: &mut StdRng,
        _input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        Bls12381G2ComputeYFromXSample(G2Affine::rand(rng))
    }
}
