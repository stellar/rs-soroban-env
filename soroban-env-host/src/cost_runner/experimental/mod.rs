#[cfg(feature = "next")]
mod decode_secp256r1_sig;
mod ecdsa_secp256k1_verify;
#[cfg(feature = "next")]
mod ecdsa_secp256r1_recover;
mod ed25519_scalar_mut;
mod read_xdr;
#[cfg(feature = "next")]
mod sec1_decode_point_compressed;

#[cfg(feature = "next")]
pub use decode_secp256r1_sig::*;
pub use ecdsa_secp256k1_verify::*;
#[cfg(feature = "next")]
pub use ecdsa_secp256r1_recover::*;
pub use ed25519_scalar_mut::*;
pub use read_xdr::*;
#[cfg(feature = "next")]
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
        }
    }
}

impl fmt::Display for ExperimentalCostType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
