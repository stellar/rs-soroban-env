use crate::{
    err,
    xdr::{ContractCostType, Hash, ScBytes, ScErrorCode, ScErrorType},
    BytesObject, Host, HostError, U32Val,
};
use sha2::Sha256;
use sha3::Keccak256;

impl Host {
    // Ed25519 functions

    pub(crate) fn ed25519_signature_from_bytes(
        &self,
        name: &'static str,
        bytes: &[u8],
    ) -> Result<ed25519_dalek::Signature, HostError> {
        self.fixed_length_bytes_from_slice::<ed25519_dalek::Signature, {ed25519_dalek::SIGNATURE_LENGTH}>(name, bytes)
    }

    pub(crate) fn ed25519_signature_from_bytesobj_input(
        &self,
        name: &'static str,
        sig: BytesObject,
    ) -> Result<ed25519_dalek::Signature, HostError> {
        self.fixed_length_bytes_from_bytesobj_input::<ed25519_dalek::Signature, {ed25519_dalek::SIGNATURE_LENGTH}>(name, sig)
    }

    pub(crate) fn ed25519_pub_key_from_bytes(
        &self,
        bytes: &[u8],
    ) -> Result<ed25519_dalek::PublicKey, HostError> {
        self.charge_budget(ContractCostType::ComputeEd25519PubKey, None)?;
        ed25519_dalek::PublicKey::from_bytes(bytes).map_err(|_| {
            err!(
                self,
                (ScErrorType::Crypto, ScErrorCode::InvalidInput),
                "invalid ed25519 public key",
                bytes
            )
        })
    }

    pub fn ed25519_pub_key_from_bytesobj_input(
        &self,
        k: BytesObject,
    ) -> Result<ed25519_dalek::PublicKey, HostError> {
        self.visit_obj(k, |bytes: &ScBytes| {
            self.ed25519_pub_key_from_bytes(bytes.as_slice())
        })
    }

    pub(crate) fn verify_sig_ed25519_internal(
        &self,
        payload: &[u8],
        public_key: &ed25519_dalek::PublicKey,
        sig: &ed25519_dalek::Signature,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("ed25519 verify");
        self.charge_budget(
            ContractCostType::VerifyEd25519Sig,
            Some(payload.len() as u64),
        )?;
        public_key.verify_strict(payload, sig).map_err(|_| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "failed ED25519 verification",
                &[],
            )
        })
    }

    // ECDSA secp256k1 functions

    pub(crate) fn secp256k1_pub_key_from_bytes(
        &self,
        bytes: &[u8],
    ) -> Result<k256::PublicKey, HostError> {
        self.charge_budget(ContractCostType::ComputeEcdsaSecp256k1Key, None)?;
        k256::PublicKey::from_sec1_bytes(bytes).map_err(|_| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "invalid ECDSA-secp256k1 public key",
                &[],
            )
        })
    }

    pub(crate) fn secp256k1_pub_key_from_bytesobj_input(
        &self,
        k: BytesObject,
    ) -> Result<k256::PublicKey, HostError> {
        self.visit_obj(k, |bytes: &ScBytes| {
            self.secp256k1_pub_key_from_bytes(bytes.as_slice())
        })
    }

    pub(crate) fn secp256k1_signature_from_bytes(
        &self,
        bytes: &[u8],
    ) -> Result<k256::ecdsa::Signature, HostError> {
        use k256::elliptic_curve::scalar::IsHigh;
        self.charge_budget(ContractCostType::ComputeEcdsaSecp256k1Sig, None)?;
        let sig: k256::ecdsa::Signature =
            k256::ecdsa::Signature::try_from(bytes).map_err(|_| {
                self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "invalid ECDSA-secp256k1 signature",
                    &[],
                )
            })?;
        if sig.s().is_high().into() {
            Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "ECDSA-secp256k1 signature 's' part is not normalized to low form",
                &[],
            ))
        } else {
            Ok(sig)
        }
    }

    pub(crate) fn secp256k1_signature_from_bytesobj_input(
        &self,
        k: BytesObject,
    ) -> Result<k256::ecdsa::Signature, HostError> {
        self.visit_obj(k, |bytes: &ScBytes| {
            self.secp256k1_signature_from_bytes(bytes.as_slice())
        })
    }

    // NB: not metered as it's a trivial constant cost, just converting a byte to a byte,
    // and always done exactly once as part of the secp256k1 recovery path.
    pub(crate) fn secp256k1_recovery_id_from_u32val(
        &self,
        recovery_id: U32Val,
    ) -> Result<k256::ecdsa::RecoveryId, HostError> {
        let rid32: u32 = u32::from(recovery_id);
        if rid32 > k256::ecdsa::RecoveryId::MAX as u32 {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "invalid ECDSA-secp256k1 recovery ID",
                &[recovery_id.to_val()],
            ));
        }
        k256::ecdsa::RecoveryId::try_from(rid32 as u8).map_err(|_| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "invalid ECDSA-secp256k1 recovery ID",
                &[recovery_id.to_val()],
            )
        })
    }

    pub(crate) fn recover_key_ecdsa_secp256k1_internal(
        &self,
        hash: &Hash,
        sig: &k256::ecdsa::Signature,
        rid: k256::ecdsa::RecoveryId,
    ) -> Result<BytesObject, HostError> {
        let _span = tracy_span!("secp256k1 recover");
        self.charge_budget(ContractCostType::RecoverEcdsaSecp256k1Key, None)?;
        let recovered_key =
            k256::ecdsa::VerifyingKey::recover_from_prehash(hash.as_slice(), &sig, rid).map_err(
                |_| {
                    self.err(
                        ScErrorType::Crypto,
                        ScErrorCode::InvalidInput,
                        "ECDSA-secp256k1 signature recovery failed",
                        &[],
                    )
                },
            )?;
        let rk = ScBytes::from(crate::xdr::BytesM::try_from(
            recovered_key.to_encoded_point(false).as_bytes(),
        )?);
        self.add_host_object(rk)
    }

    // SHA256 functions

    pub(crate) fn sha256_hash_from_bytes(&self, bytes: &[u8]) -> Result<Vec<u8>, HostError> {
        let _span = tracy_span!("sha256");
        self.charge_budget(
            ContractCostType::ComputeSha256Hash,
            Some(bytes.len() as u64),
        )?;
        Ok(<Sha256 as sha2::Digest>::digest(bytes).as_slice().to_vec())
    }

    pub fn sha256_hash_from_bytesobj_input(&self, x: BytesObject) -> Result<Vec<u8>, HostError> {
        self.visit_obj(x, |bytes: &ScBytes| {
            let hash = self.sha256_hash_from_bytes(bytes.as_slice())?;
            if hash.len() != 32 {
                return Err(err!(
                    self,
                    (ScErrorType::Object, ScErrorCode::UnexpectedSize),
                    "expected 32-byte BytesObject for hash, got different size",
                    hash.len()
                ));
            }
            Ok(hash)
        })
    }

    // Keccak256/SHA3 functions

    pub(crate) fn keccak256_hash_from_bytes(&self, bytes: &[u8]) -> Result<Vec<u8>, HostError> {
        let _span = tracy_span!("keccak256");
        self.charge_budget(
            ContractCostType::ComputeKeccak256Hash,
            Some(bytes.len() as u64),
        )?;
        Ok(<Keccak256 as sha3::Digest>::digest(bytes)
            .as_slice()
            .to_vec())
    }

    pub(crate) fn keccak256_hash_from_bytesobj_input(
        &self,
        x: BytesObject,
    ) -> Result<Vec<u8>, HostError> {
        self.visit_obj(x, |bytes: &ScBytes| {
            let hash = self.keccak256_hash_from_bytes(bytes.as_slice())?;
            if hash.len() != 32 {
                return Err(err!(
                    self,
                    (ScErrorType::Object, ScErrorCode::UnexpectedSize),
                    "expected 32-byte BytesObject for hash, got different size",
                    hash.len()
                ));
            }
            Ok(hash)
        })
    }
}
