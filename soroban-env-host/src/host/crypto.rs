use crate::host::prng::SEED_BYTES;
use crate::{
    budget::AsBudget,
    err,
    xdr::{ContractCostType, Hash, ScBytes, ScErrorCode, ScErrorType},
    BytesObject, Error, Host, HostError, U32Val, Val,
};
use hex_literal::hex;
use hmac::{Hmac, Mac};
use rand::RngCore;
use rand_chacha::ChaCha20Rng;
use sha2::Sha256;
use sha3::Keccak256;

use super::metered_clone::MeteredContainer;

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

    pub(crate) fn ed25519_pub_key_from_bytesobj_input(
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
            recovered_key
                .to_encoded_point(/*compress:*/ false)
                .as_bytes(),
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
                    "expected 32-byte BytesObject for sha256 hash, got different size",
                    hash.len()
                ));
            }
            Ok(hash)
        })
    }

    // Keccak256/SHA3 functions
    pub(crate) fn keccak256_hash_from_bytes_raw(
        &self,
        bytes: &[u8],
    ) -> Result<[u8; 32], HostError> {
        let _span = tracy_span!("keccak256");
        self.charge_budget(
            ContractCostType::ComputeKeccak256Hash,
            Some(bytes.len() as u64),
        )?;
        Ok(<Keccak256 as sha3::Digest>::digest(bytes).into())
    }

    pub(crate) fn keccak256_hash_from_bytes(&self, bytes: &[u8]) -> Result<Vec<u8>, HostError> {
        Vec::<u8>::charge_bulk_init_cpy(32, self.as_budget())?;
        self.keccak256_hash_from_bytes_raw(bytes)
            .map(|x| x.to_vec())
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
                    "expected 32-byte BytesObject for keccak256 hash, got different size",
                    hash.len()
                ));
            }
            Ok(hash)
        })
    }
}

pub(crate) fn sha256_hash_from_bytes_raw(
    bytes: &[u8],
    budget: impl AsBudget,
) -> Result<[u8; 32], HostError> {
    let _span = tracy_span!("sha256");
    budget.as_budget().charge(
        ContractCostType::ComputeSha256Hash,
        Some(bytes.len() as u64),
    )?;
    Ok(<Sha256 as sha2::Digest>::digest(bytes).into())
}

pub(crate) fn sha256_hash_from_bytes(
    bytes: &[u8],
    budget: impl AsBudget,
) -> Result<Vec<u8>, HostError> {
    Vec::<u8>::charge_bulk_init_cpy(32, budget.clone())?;
    sha256_hash_from_bytes_raw(bytes, budget).map(|x| x.to_vec())
}

pub(crate) fn chacha20_fill_bytes(
    rng: &mut ChaCha20Rng,
    dest: &mut [u8],
    budget: impl AsBudget,
) -> Result<(), HostError> {
    tracy_span!("chacha20");
    budget
        .as_budget()
        .charge(ContractCostType::ChaCha20DrawBytes, Some(dest.len() as u64))?;
    rng.fill_bytes(dest);
    Ok(())
}

// It is possible that a user-provided PRNG seed (either in a test or, more
// worryingly, in a production environment) is biased: it might be all zero, or
// all copies of a single byte, or otherwise statistically unlike a uniformly
// random bitstream with roughly 50-50 zero and one bits.
//
// Unfortunately the security properties of the stream cipher ChaCha used in the
// PRNG (being "indistinguishable from uniform random") are based on the
// assumption of an _unbiased_ seed.
//
// So we run any seed through HMAC-SHA256 here, with a constant uniform random
// salt, as an unbiasing step (this is the "randomness-extractor" phase of HKDF,
// which is the only part relevant to our needs, we don't need multiple keys).

pub(crate) fn unbias_prng_seed(
    seed: &[u8; SEED_BYTES as usize],
    budget: impl AsBudget,
) -> Result<[u8; SEED_BYTES as usize], HostError> {
    tracy_span!("unbias_prng_seed");

    // Salt is fixed and must not be changed; it is effectively "part of the
    // protocol" and must be the same for all implementations.
    //
    // Note: salt is a "public random value", intended to be statistically
    // similar to a 32-byte draw on /dev/random but done in a transparent and
    // reproducible way. In this case we use the Stellar Public Network ID,
    // `sha256("Public Global Stellar Network ; September 2015")`.
    //
    // This number as a bitstring has 137 zeroes and 119 ones, which is within
    // the range we get when taking 32-byte samples from /dev/random (feel free
    // to check this yourself).

    const SALT: [u8; 32] = hex!("7ac33997544e3175d266bd022439b22cdb16508c01163f26e5cb2a3e1045a979");

    // Running HMAC will run SHA256 2 times on 64 bytes each time (32-byte salt
    // concatenated with 32-byte input).
    budget
        .as_budget()
        .bulk_charge(ContractCostType::ComputeSha256Hash, 2, Some(64))?;

    let mut hmac = Hmac::<Sha256>::new_from_slice(&SALT)
        .map_err(|_| Error::from_type_and_code(ScErrorType::Context, ScErrorCode::InternalError))?;
    hmac.update(seed);
    Ok(hmac.finalize().into_bytes().into())
}
