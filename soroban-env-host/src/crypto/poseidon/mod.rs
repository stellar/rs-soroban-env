// Poseidon hash function implementations
//
// This module contains implementations of both Poseidon and Poseidon2 hash functions
// for use in cryptographic operations within the Soroban environment.

pub mod poseidon;
pub mod poseidon2;
pub mod poseidon2_params;
pub mod poseidon_params;
mod utils;

pub use poseidon::Poseidon;
pub use poseidon2::Poseidon2;
pub use poseidon2_params::Poseidon2Params;
pub use poseidon_params::PoseidonParams;

// Both BLS12-381 and BN254 require sbox degree to be 5, and these are the only
// two fields we currently support.
pub(crate) const SUPPORTED_SBOX_DEGREES: [u32; 1] = [5];

use crate::{
    xdr::{ScErrorCode, ScErrorType},
    Error,
};
pub(crate) const VEC_OOB: Error =
    Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds);
pub(crate) const INVALID_INPUT: Error =
    Error::from_type_and_code(ScErrorType::Crypto, ScErrorCode::InvalidInput);
