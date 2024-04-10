#![allow(dead_code)]
use crate::host::prng::SEED_BYTES;
use crate::{
    budget::AsBudget,
    err,
    xdr::{ContractCostType, Hash, ScBytes, ScErrorCode, ScErrorType},
    BytesObject, Error, Host, HostError, U32Val, Val,
};
use blst::{
    blst_fr, blst_fr_from_scalar, blst_lendian_from_scalar, blst_p1, blst_p1_add, blst_p1_affine,
    blst_p1_affine_in_g1, blst_p1_from_affine, blst_p1_mult, blst_p1_on_curve, blst_p1_serialize,
    blst_p1_uncompress, blst_p2, blst_p2_add, blst_p2_affine, blst_p2_affine_in_g2,
    blst_p2_deserialize, blst_p2_from_affine, blst_p2_mult, blst_p2_on_curve, blst_p2_serialize,
    blst_scalar, blst_scalar_fr_check, blst_scalar_from_fr,
    blst_scalar_from_lendian, BLST_ERROR,
};
use hex_literal::hex;
use hmac::{Hmac, Mac};
use rand::RngCore;
use rand_chacha::ChaCha20Rng;
use sha2::Sha256;
use sha3::Keccak256;

use super::metered_clone::MeteredContainer;

pub const BLST_G1_UNCOMPRESSED_SIZE: usize = 96;
pub const BLST_G2_UNCOMPRESSED_SIZE: usize = 192;
pub const BLS_SCALAR_SIZE: usize = 32;
pub const BLST_RESULT_SIZE: usize = 255;

impl Host {
    // Ed25519 functions
    pub(crate) fn ed25519_signature_from_bytesobj_input(
        &self,
        name: &'static str,
        sig: BytesObject,
    ) -> Result<ed25519_dalek::Signature, HostError> {
        self.fixed_length_bytes_from_bytesobj_input::<ed25519_dalek::Signature, { ed25519_dalek::SIGNATURE_LENGTH }>(name, sig)
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

    pub(crate) fn bls_g1_add_raw_internal(
        &self,
        p0: &[u8; BLST_G1_UNCOMPRESSED_SIZE],
        p1: &[u8; BLST_G1_UNCOMPRESSED_SIZE],
    ) -> Result<[u8; BLST_G1_UNCOMPRESSED_SIZE], HostError> {
        let p0 = decode_p1(p0).ok_or_else(|| self.err(
            ScErrorType::Crypto,
            ScErrorCode::InvalidInput,
            "Failed to decode p0",
            &[],
        ))?;
        let p1 = decode_p1(p1).ok_or_else(|| self.err(
            ScErrorType::Crypto,
            ScErrorCode::InvalidInput,
            "Failed to decode p1",
            &[],
        ))?;
        let mut res = blst_p1::default();
        let mut out = [0u8; BLST_G1_UNCOMPRESSED_SIZE];

        unsafe { blst_p1_add(&mut res, &p0, &p1) };

        unsafe {
            blst_p1_serialize(out.as_mut_ptr(), &res);
        }

        Ok(out)
    }

    pub(crate) fn bls_g1_mul_raw_internal(
        &self,
        scalar: &[u8; BLS_SCALAR_SIZE],
        p1: &[u8; BLST_G1_UNCOMPRESSED_SIZE],
    ) -> Result<[u8; BLST_G1_UNCOMPRESSED_SIZE], HostError> {
        let p1 = decode_p1(p1).ok_or_else(|| self.err(
            ScErrorType::Crypto,
            ScErrorCode::InvalidInput,
            "Failed to decode p1",
            &[],
        ))?;
        let scalar_ftr = scalar_fr_from_bytes(scalar).ok_or_else(|| self.err(
            ScErrorType::Crypto,
            ScErrorCode::InvalidInput,
            "Failed to decode scalar",
            &[],
        ))?;
        let scalar = bls_fr_to_bytes(scalar_ftr);
        let mut res = blst_p1::default();
        let mut out = [0u8; BLST_G1_UNCOMPRESSED_SIZE];
        unsafe { blst_p1_mult(&mut res, &p1, scalar.as_ptr(), BLST_RESULT_SIZE) };

        unsafe {
            blst_p1_serialize(out.as_mut_ptr(), &res);
        }

        Ok(out)
    }

    pub(crate) fn bls_g1_mul_raw_exp(
        &self,
        scalars: &[u8],
        p_n: &[u8],
    ) -> Result<[u8; BLST_G1_UNCOMPRESSED_SIZE], HostError> {
        if let Some(value) = self.validate_points_input(&p_n, BLST_G1_UNCOMPRESSED_SIZE) {
            return Err(value);
        }

        let mut res = blst_p1::default();
        let mut out = [0u8; BLST_G1_UNCOMPRESSED_SIZE];
        for (i, chunk) in p_n.chunks_exact(BLST_G1_UNCOMPRESSED_SIZE).enumerate() {
            let p1 = decode_p1(chunk.try_into().unwrap()).ok_or_else(|| self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "Failed to decode chunk",
                &[],
            ))?;
            let mut tmp = blst_p1::default();
            let scalar = scalars[i];
            unsafe { blst_p1_mult(&mut tmp, &p1, &scalar, BLST_RESULT_SIZE) };
            unsafe { blst_p1_add(&mut res, &res, &tmp) };
        }

        unsafe {
            blst_p1_serialize(out.as_mut_ptr(), &res);
        }

        Ok(out)
    }

    pub(crate) fn bls_g2_add_raw_internal(
        &self,
        p0: &[u8; BLST_G2_UNCOMPRESSED_SIZE],
        p1: &[u8; BLST_G2_UNCOMPRESSED_SIZE],
    ) -> Result<[u8; BLST_G2_UNCOMPRESSED_SIZE], HostError> {
        let p0 = decode_p2(p0).ok_or_else(|| self.err(
            ScErrorType::Crypto,
            ScErrorCode::InvalidInput,
            "Failed to decode p0",
            &[],
        ))?;
        let p1 = decode_p2(p1).ok_or_else(|| self.err(
            ScErrorType::Crypto,
            ScErrorCode::InvalidInput,
            "Failed to decode p1",
            &[],
        ))?;
        let mut res = blst_p2::default();
        let mut out = [0u8; BLST_G2_UNCOMPRESSED_SIZE];

        unsafe { blst_p2_add(&mut res, &p0, &p1) };

        unsafe {
            blst_p2_serialize(out.as_mut_ptr(), &res);
        }

        Ok(out)
    }

    pub(crate) fn bls_g2_mul_raw_internal(
        &self,
        scalar: &[u8; BLS_SCALAR_SIZE],
        p1: &[u8; BLST_G2_UNCOMPRESSED_SIZE],
    ) -> Result<[u8; BLST_G2_UNCOMPRESSED_SIZE], HostError> {
        let p1 = decode_p2(p1).ok_or_else(|| self.err(
            ScErrorType::Crypto,
            ScErrorCode::InvalidInput,
            "Failed to decode p1",
            &[],
        ))?;
        let scalar_ftr = scalar_fr_from_bytes(scalar).ok_or_else(|| self.err(
            ScErrorType::Crypto,
            ScErrorCode::InvalidInput,
            "Failed to decode scalar",
            &[],
        ))?;
        let scalar = bls_fr_to_bytes(scalar_ftr);
        let mut res = blst_p2::default();
        let mut out = [0u8; BLST_G2_UNCOMPRESSED_SIZE];
        unsafe { blst_p2_mult(&mut res, &p1, scalar.as_ptr(), BLST_RESULT_SIZE) };

        unsafe {
            blst_p2_serialize(out.as_mut_ptr(), &res);
        }
        Ok(out)
    }

    pub(crate) fn bls_g2_mul_raw_exp_internal(
        &self,
        scalars: &[u8],
        p_n: &[u8],
    ) -> Result<[u8; BLST_G2_UNCOMPRESSED_SIZE], HostError> {
        if let Some(value) = self.validate_points_input(&p_n, BLST_G2_UNCOMPRESSED_SIZE) {
            return Err(value);
        }

        let mut res = blst_p2::default();
        let mut out = [0u8; BLST_G2_UNCOMPRESSED_SIZE];
        for (i, chunk) in p_n.chunks_exact(BLST_G2_UNCOMPRESSED_SIZE).enumerate() {
            let p2 = decode_p2(chunk.try_into().unwrap()).ok_or_else(|| self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "Failed to decode chunk",
                &[],
            ))?;
            let mut tmp = blst_p2::default();
            let scalar = scalars[i];
            unsafe { blst_p2_mult(&mut tmp, &p2, &scalar, BLST_RESULT_SIZE) };
            unsafe { blst_p2_add(&mut res, &res, &tmp) };
        }

        unsafe {
            blst_p2_serialize(out.as_mut_ptr(), &res);
        }

        Ok(out)
    }
    fn validate_points_input(&self, p_n: &&[u8], size: usize) -> Option<HostError> {
        if p_n.len() % size != 0 {
            return Some(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("number of points bytes should divisible by {}", size).as_str(),
                &[Val::from_u32(p_n.len() as u32).into()],
            ));
        }
        None
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

pub fn bls_fr_to_bytes(scalar_fr: blst_fr) -> Option<[u8; BLS_SCALAR_SIZE]> {
    let mut scalar = blst_scalar::default();
    let mut out = [0u8; BLS_SCALAR_SIZE];

    unsafe {
        blst_scalar_from_fr(&mut scalar, &scalar_fr);
        blst_lendian_from_scalar(out.as_mut_ptr(), &scalar);
    }

    Some(out)
}

pub fn scalar_fr_from_bytes(bytes: &[u8; BLS_SCALAR_SIZE]) -> Option<blst_fr> {
    let mut scalar = blst_scalar::default();
    unsafe {
        blst_scalar_from_lendian(&mut scalar, bytes.as_ptr());
        if blst_scalar_fr_check(&scalar) {
            let mut fr = blst_fr::default();
            blst_fr_from_scalar(&mut fr, &scalar);
            Some(fr)
        } else {
            None
        }
    }
}

pub fn decode_p1_affine(bytes: &[u8; BLST_G1_UNCOMPRESSED_SIZE]) -> Option<blst_p1_affine> {
    let mut raw = blst_p1_affine::default();
    unsafe {
        if blst_p1_uncompress(&mut raw, bytes.as_ptr()) == BLST_ERROR::BLST_SUCCESS && blst_p1_affine_in_g1(&raw) {
            Some(raw)
        } else {
            None
        }
    }
}

pub fn decode_p1(bytes: &[u8; BLST_G1_UNCOMPRESSED_SIZE]) -> Option<blst_p1> {
    decode_p1_affine(bytes).and_then(|p1_affine| {
        let mut raw = blst_p1::default();
        unsafe {
            blst_p1_from_affine(&mut raw, &p1_affine);
            if blst_p1_on_curve(&raw) {
                Some(raw)
            } else {
                None
            }
        }
    })
}

pub fn decode_p2_affine(bytes: &[u8; BLST_G2_UNCOMPRESSED_SIZE]) -> Option<blst_p2_affine> {
    let mut raw = blst_p2_affine::default();
    unsafe {
        if blst_p2_deserialize(&mut raw, bytes.as_ptr()) == BLST_ERROR::BLST_SUCCESS && blst_p2_affine_in_g2(&raw) {
            Some(raw)
        } else {
            None
        }
    }
}

pub fn decode_p2(bytes: &[u8; BLST_G2_UNCOMPRESSED_SIZE]) -> Option<blst_p2> {
    decode_p2_affine(bytes).and_then(|p2_affine| {
        let mut raw = blst_p2::default();
        unsafe {
            blst_p2_from_affine(&mut raw, &p2_affine);
            if blst_p2_on_curve(&raw) {
                Some(raw)
            } else {
                None
            }
        }
    })
}
