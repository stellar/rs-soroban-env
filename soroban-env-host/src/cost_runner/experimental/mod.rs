mod bls12_381;
mod decode_secp256r1_sig;
mod ecdsa_secp256k1_verify;
mod ecdsa_secp256r1_recover;
mod ed25519_scalar_mut;
mod read_xdr;
mod sec1_decode_point_compressed;

pub use bls12_381::*;
pub use decode_secp256r1_sig::*;
pub use ecdsa_secp256k1_verify::*;
pub use ecdsa_secp256r1_recover::*;
pub use ed25519_scalar_mut::*;
pub use read_xdr::*;
pub use sec1_decode_point_compressed::*;

use crate::xdr::Name;
use core::fmt;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExperimentalCostType {
    EdwardsPointCurve25519ScalarMul,
    ReadXdrByteArray,
    EcdsaSecp256r1Recover,
    Sec1DecodePointCompressed,
    DecodeSecp256r1Signature,
    EcdsaSecp256k1Verify,
    Bls12381G1AffineDeserializeUncompressed,
    Bls12381G1AffineSerializeUncompressed,
    Bls12381G2AffineDeserializeUncompressed,
    Bls12381G2AffineSerializeUncompressed,
    Bls12381Fp2DeserializeUncompressed,
    Bls12381G1ComputeYFromX,
    Bls12381G2ComputeYFromX,
}

impl Name for ExperimentalCostType {
    fn name(&self) -> &'static str {
        match self {
            ExperimentalCostType::EdwardsPointCurve25519ScalarMul => {
                "EdwardsPointCurve25519ScalarMul"
            }
            ExperimentalCostType::ReadXdrByteArray => "ReadXdrByteArray",
            ExperimentalCostType::EcdsaSecp256r1Recover => "EcdsaSecp256r1Recover",
            ExperimentalCostType::Sec1DecodePointCompressed => "Sec1DecodePointCompressed",
            ExperimentalCostType::DecodeSecp256r1Signature => "DecodeSecp256r1Signature",
            ExperimentalCostType::EcdsaSecp256k1Verify => "EcdsaSecp256k1Verify",
            ExperimentalCostType::Bls12381G1AffineDeserializeUncompressed => {
                "Bls12381G1AffineDeserializeUncompressed"
            }
            ExperimentalCostType::Bls12381G1AffineSerializeUncompressed => {
                "Bls12381G1AffineSerializeUncompressed"
            }
            ExperimentalCostType::Bls12381G2AffineDeserializeUncompressed => {
                "Bls12381G2AffineDeserializeUncompressed"
            }
            ExperimentalCostType::Bls12381G2AffineSerializeUncompressed => {
                "Bls12381G2AffineSerializeUncompressed"
            }
            ExperimentalCostType::Bls12381Fp2DeserializeUncompressed => {
                "Bls12381Fp2DeserializeUncompressed"
            }
            ExperimentalCostType::Bls12381G1ComputeYFromX => "Bls12381G1ComputeYFromX",
            ExperimentalCostType::Bls12381G2ComputeYFromX => "Bls12381G2ComputeYFromX",
        }
    }
}

impl fmt::Display for ExperimentalCostType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
