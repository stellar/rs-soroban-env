use ecdsa::{signature::hazmat::PrehashVerifier, Signature};
use k256::Secp256k1;

use crate::{
    xdr::{Hash, ScErrorCode, ScErrorType},
    Host, HostError,
};

impl Host {
    pub(crate) fn ecdsa_secp256k1_verify_signature(
        &self,
        verifying_key: &k256::ecdsa::VerifyingKey,
        msg_hash: &Hash,
        sig: &Signature<Secp256k1>,
    ) -> Result<(), HostError> {
        verifying_key
            .verify_prehash(msg_hash.as_slice(), sig)
            .map_err(|_| {
                self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "failed secp256r1 verification",
                    &[],
                )
            })
    }

    pub(crate) fn ecdsa_secp256r1_recover_key(
        &self,
        msg_hash: &Hash,
        sig: &Signature<p256::NistP256>,
        rid: ecdsa::RecoveryId,
    ) -> Result<crate::xdr::ScBytes, HostError> {
        let recovered_key =
            p256::ecdsa::VerifyingKey::recover_from_prehash(msg_hash.as_slice(), &sig, rid)
                .map_err(|_| {
                    self.err(
                        ScErrorType::Crypto,
                        ScErrorCode::InvalidInput,
                        "ECDSA-secp256k1 signature recovery failed",
                        &[],
                    )
                })?;
        Ok(crate::xdr::ScBytes::from(crate::xdr::BytesM::try_from(
            recovered_key
                .to_encoded_point(/*compress:*/ false)
                .as_bytes(),
        )?))
    }
}
