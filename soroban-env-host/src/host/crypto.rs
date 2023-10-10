use crate::{
    budget::AsBudget,
    err,
    xdr::{ContractCostType, Hash, ScBytes, ScErrorCode, ScErrorType},
    BytesObject, Host, HostError, U32Val, Val,
};
use rand::RngCore;
use rand_chacha::ChaCha20Rng;
use sha2::Sha256;
use sha3::Keccak256;

impl Host {
    // Ed25519 functions
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
    ) -> Result<ed25519_dalek::VerifyingKey, HostError> {
        self.charge_budget(ContractCostType::ComputeEd25519PubKey, None)?;
        let vk_bytes = bytes.try_into().map_err(|_| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "invalid length of ed25519 public key",
                &[Val::from_u32(bytes.len() as u32).into()],
            )
        })?;
        ed25519_dalek::VerifyingKey::from_bytes(vk_bytes).map_err(|_| {
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
    ) -> Result<ed25519_dalek::VerifyingKey, HostError> {
        self.visit_obj(k, |bytes: &ScBytes| {
            self.ed25519_pub_key_from_bytes(bytes.as_slice())
        })
    }

    pub(crate) fn verify_sig_ed25519_internal(
        &self,
        payload: &[u8],
        verifying_key: &ed25519_dalek::VerifyingKey,
        sig: &ed25519_dalek::Signature,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("ed25519 verify");
        self.charge_budget(
            ContractCostType::VerifyEd25519Sig,
            Some(payload.len() as u64),
        )?;
        verifying_key.verify_strict(payload, sig).map_err(|_| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "failed ED25519 verification",
                &[],
            )
        })
    }

    // ECDSA secp256k1 functions

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

    pub(crate) fn sha256_hash_from_bytesobj_input(
        &self,
        x: BytesObject,
    ) -> Result<Vec<u8>, HostError> {
        self.visit_obj(x, |bytes: &ScBytes| {
            let hash = sha256_hash_from_bytes(bytes.as_slice(), self)?;
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

pub(crate) fn sha256_hash_from_bytes(
    bytes: &[u8],
    budget: impl AsBudget,
) -> Result<Vec<u8>, HostError> {
    let _span = tracy_span!("sha256");
    budget.as_budget().charge(
        ContractCostType::ComputeSha256Hash,
        Some(bytes.len() as u64),
    )?;
    Ok(<Sha256 as sha2::Digest>::digest(bytes).as_slice().to_vec())
}

pub(crate) fn chacha20_fill_bytes(
    rng: &mut ChaCha20Rng,
    dest: &mut [u8],
    budget: impl AsBudget,
) -> Result<(), HostError> {
    budget
        .as_budget()
        .charge(ContractCostType::ChaCha20DrawBytes, Some(dest.len() as u64))?;
    rng.fill_bytes(dest);
    Ok(())
}
