use crate::common::HostCostMeasurement;
use ark_bls12_381::{Fq2, G1Affine, G2Affine};
use ark_ff::UniformRand;
use ark_serialize::CanonicalSerialize;
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::{
        Bls12381Fp2DeserializeUncompressedRun, Bls12381G1AffineDeserializeUncompressedRun,
        Bls12381G1AffineSerializeUncompressedRun, Bls12381G2AffineDeserializeUncompressedRun,
        Bls12381G2AffineSerializeUncompressedRun,
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
