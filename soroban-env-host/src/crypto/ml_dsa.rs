//! ML-DSA (FIPS 204) signature verification.
//!
//! Wraps the RustCrypto `ml-dsa` crate. One generic implementation over
//! `P: MlDsaVariant` serves all three parameter sets (ML-DSA-44/65/87); the
//! per-variant host functions in host.rs are thin monomorphizing wrappers.
//! `MlDsaVariant` carries everything that differs between parameter sets --
//! encoded lengths, cost types, and the name used in diagnostics -- so the
//! shared code never takes them as arguments.
//!
//! Metering follows CAP-0087: three cost types per parameter set, covering
//! verifying-key decoding, signature decoding, and verification proper. Each
//! is charged after its input has been length-checked and before the
//! corresponding work is performed. The cost model parameters are
//! uncalibrated placeholders; the calibration benchmarks live in
//! `cost_runner/cost_types/ml_dsa.rs`.

use crate::{
    err,
    xdr::{ContractCostType, ScBytes, ScErrorCode, ScErrorType},
    BytesObject, Host, HostError,
};
use ml_dsa::{
    EncodedSignature, EncodedVerifyingKey, MlDsa44, MlDsa65, MlDsa87, MlDsaParams, Signature,
    VerifyingKey,
};

/// FIPS 204 limits the context string to 255 bytes, for every parameter set.
pub(crate) const ML_DSA_MAX_CONTEXT_LEN: usize = 255;

/// One ML-DSA parameter set, with the encoded lengths CAP-0087 specifies, the
/// cost types a verification charges, and the name that identifies it in error
/// messages.
///
/// The encoded lengths are the values declared by the spec rather than
/// whatever the underlying crate happens to use; `declare_ml_dsa_variant!`
/// statically asserts the two agree.
pub(crate) trait MlDsaVariant: MlDsaParams {
    /// Parameter set name, e.g. "ML-DSA-44". Included in every error this
    /// module raises, so a diagnostic always names the exact variant.
    const NAME: &'static str;
    /// Length of a `pkEncode` verifying key (FIPS 204 Algorithm 22).
    const VERIFYING_KEY_LEN: usize;
    /// Length of a `sigEncode` signature (FIPS 204 Algorithm 26).
    const SIGNATURE_LEN: usize;
    const DECODE_VERIFYING_KEY_COST: ContractCostType;
    const DECODE_SIGNATURE_COST: ContractCostType;
    const VERIFY_SIG_COST: ContractCostType;
}

macro_rules! declare_ml_dsa_variant {
    (
        $p:ty, $name:literal, $vk_len:literal, $sig_len:literal,
        $decode_vk_cost:ident, $decode_sig_cost:ident, $verify_cost:ident
    ) => {
        impl MlDsaVariant for $p {
            const NAME: &'static str = $name;
            const VERIFYING_KEY_LEN: usize = $vk_len;
            const SIGNATURE_LEN: usize = $sig_len;
            const DECODE_VERIFYING_KEY_COST: ContractCostType = ContractCostType::$decode_vk_cost;
            const DECODE_SIGNATURE_COST: ContractCostType = ContractCostType::$decode_sig_cost;
            const VERIFY_SIG_COST: ContractCostType = ContractCostType::$verify_cost;
        }

        // The lengths above are the spec's, not the crate's. Tie them together
        // at compile time so a crate that ever disagreed is a build failure
        // rather than a runtime rejection reported with the wrong size.
        static_assertions::assert_eq_size!(EncodedVerifyingKey<$p>, [u8; $vk_len]);
        static_assertions::assert_eq_size!(EncodedSignature<$p>, [u8; $sig_len]);
    };
}

// k=4, l=4.
declare_ml_dsa_variant!(
    MlDsa44,
    "ML-DSA-44",
    1312,
    2420,
    MlDsa44DecodeVerifyingKey,
    MlDsa44DecodeSignature,
    VerifyMlDsa44Sig
);
// k=6, l=5.
declare_ml_dsa_variant!(
    MlDsa65,
    "ML-DSA-65",
    1952,
    3309,
    MlDsa65DecodeVerifyingKey,
    MlDsa65DecodeSignature,
    VerifyMlDsa65Sig
);
// k=8, l=7.
declare_ml_dsa_variant!(
    MlDsa87,
    "ML-DSA-87",
    2592,
    4627,
    MlDsa87DecodeVerifyingKey,
    MlDsa87DecodeSignature,
    VerifyMlDsa87Sig
);

impl Host {
    /// Decodes an ML-DSA verifying key from a byte slice. The slice length
    /// must exactly match the variant's encoded key size. The charge covers
    /// the SHAKE-128 expansion of `A_hat` and its NTT-domain precompute, and
    /// is applied only once the length has been validated.
    pub(crate) fn ml_dsa_verifying_key_from_slice<P: MlDsaVariant>(
        &self,
        bytes: &[u8],
    ) -> Result<VerifyingKey<P>, HostError> {
        if bytes.len() != P::VERIFYING_KEY_LEN {
            return Err(err!(
                self,
                (ScErrorType::Crypto, ScErrorCode::InvalidInput),
                "invalid verifying key length",
                P::NAME,
                P::VERIFYING_KEY_LEN as u64,
                bytes.len() as u64
            ));
        }
        self.charge_budget(P::DECODE_VERIFYING_KEY_COST, None)?;
        // Unreachable: the length was just checked against the spec, and the
        // static assertion above ties that to the encoded type's size.
        let enc: &EncodedVerifyingKey<P> = bytes.try_into().map_err(|_| {
            err!(
                self,
                (ScErrorType::Context, ScErrorCode::InternalError),
                "verifying key length passed validation but failed conversion",
                P::NAME
            )
        })?;
        Ok(VerifyingKey::<P>::decode(enc))
    }

    /// Decodes an ML-DSA signature from a byte slice. The slice length must
    /// exactly match the variant's encoded signature size, and the signature
    /// must be well-formed (valid hint encoding, z within range). Charging
    /// here, separately from verification, means a structurally invalid
    /// signature is rejected having paid only the decoding cost.
    pub(crate) fn ml_dsa_signature_from_slice<P: MlDsaVariant>(
        &self,
        bytes: &[u8],
    ) -> Result<Signature<P>, HostError> {
        if bytes.len() != P::SIGNATURE_LEN {
            return Err(err!(
                self,
                (ScErrorType::Crypto, ScErrorCode::InvalidInput),
                "invalid signature length",
                P::NAME,
                P::SIGNATURE_LEN as u64,
                bytes.len() as u64
            ));
        }
        self.charge_budget(P::DECODE_SIGNATURE_COST, None)?;
        // Unreachable, as above.
        let enc: &EncodedSignature<P> = bytes.try_into().map_err(|_| {
            err!(
                self,
                (ScErrorType::Context, ScErrorCode::InternalError),
                "signature length passed validation but failed conversion",
                P::NAME
            )
        })?;
        Signature::<P>::decode(enc).ok_or_else(|| {
            err!(
                self,
                (ScErrorType::Crypto, ScErrorCode::InvalidInput),
                "malformed signature",
                P::NAME
            )
        })
    }

    /// Verifies a decoded ML-DSA signature over `msg` under the context
    /// string `ctx`. Slice-based so the cost runners can drive it directly.
    ///
    /// The charge is linear in the combined message and context length: the
    /// SHAKE-256 absorption computing the message representative `mu` is the
    /// only input-dependent component, and it absorbs
    /// `0x00 || len(ctx) || ctx || M`.
    pub(crate) fn ml_dsa_verify_with_context<P: MlDsaVariant>(
        &self,
        vk: &VerifyingKey<P>,
        msg: &[u8],
        ctx: &[u8],
        sig: &Signature<P>,
    ) -> Result<(), HostError> {
        if ctx.len() > ML_DSA_MAX_CONTEXT_LEN {
            return Err(err!(
                self,
                (ScErrorType::Crypto, ScErrorCode::InvalidInput),
                "context is too long",
                P::NAME,
                ML_DSA_MAX_CONTEXT_LEN as u64,
                ctx.len() as u64
            ));
        }
        self.charge_budget(P::VERIFY_SIG_COST, Some((msg.len() + ctx.len()) as u64))?;
        if vk.verify_with_context(msg, ctx, sig) {
            Ok(())
        } else {
            Err(err!(
                self,
                (ScErrorType::Crypto, ScErrorCode::InvalidInput),
                "failed verification",
                P::NAME
            ))
        }
    }

    /// FIPS 204 ML-DSA.Verify (external interface, with context string).
    /// Shared generic implementation behind verify_sig_ml_dsa_{44,65,87}.
    pub(crate) fn ml_dsa_verify_sig_with_context<P: MlDsaVariant>(
        &self,
        public_key: BytesObject,
        msg: BytesObject,
        signature: BytesObject,
        context: BytesObject,
    ) -> Result<(), HostError> {
        let vk = self.visit_obj(public_key, |pk: &ScBytes| {
            self.ml_dsa_verifying_key_from_slice::<P>(pk.as_slice())
        })?;
        let sig = self.visit_obj(signature, |s: &ScBytes| {
            self.ml_dsa_signature_from_slice::<P>(s.as_slice())
        })?;
        self.visit_obj(msg, |m: &ScBytes| {
            self.visit_obj(context, |ctx: &ScBytes| {
                self.ml_dsa_verify_with_context::<P>(&vk, m.as_slice(), ctx.as_slice(), &sig)
            })
        })
    }
}
