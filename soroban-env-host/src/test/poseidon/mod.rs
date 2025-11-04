#[allow(clippy::module_inception)]
pub mod poseidon;
pub mod poseidon2;

// Test instances with reference test vectors
//
// Poseidon and Poseidon2 test vectors from HorizenLabs:
// Source: https://github.com/HorizenLabs/poseidon2
// These provide comprehensive test coverage for both hash functions
// across BLS12-381 and BN254 scalar fields.
pub mod poseidon2_instance_bls12;
pub mod poseidon2_instance_bn254;
pub mod poseidon_instance_bls12;
pub mod poseidon_instance_bn254;

// Additional Poseidon test vectors from the original Hades hash implementation:
// Source: https://extgit.isec.tugraz.at/krypto/hadeshash
// The hadeshash repository contains the reference implementation of Poseidon.
pub mod poseidon_instance_hadeshash_bls12;
pub mod poseidon_instance_hadeshash_bn254;
