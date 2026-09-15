use ml_dsa::{
    EncodedSignature, EncodedVerifyingKey, MlDsa44, MlDsa65, MlDsa87, MlDsaParams, Signature,
    VerifyingKey,
};

use crate::{
    err,
    xdr::{ContractCostType, ScBytes, ScErrorCode, ScErrorType},
    BytesObject, Host, HostError,
};

pub(crate) const ML_DSA_MAX_CONTEXT_LEN: usize = 255;

/// Binds each parameter set to its own cost types, so a verification can never
/// be metered against a different parameter set's schedule.
pub(crate) trait MlDsaCostTypes {
    const DECODE_VERIFYING_KEY: ContractCostType;
    const DECODE_SIGNATURE: ContractCostType;
    const VERIFY: ContractCostType;
}

impl MlDsaCostTypes for MlDsa44 {
    const DECODE_VERIFYING_KEY: ContractCostType = ContractCostType::MlDsa44DecodeVerifyingKey;
    const DECODE_SIGNATURE: ContractCostType = ContractCostType::MlDsa44DecodeSignature;
    const VERIFY: ContractCostType = ContractCostType::VerifyMlDsa44Sig;
}

impl MlDsaCostTypes for MlDsa65 {
    const DECODE_VERIFYING_KEY: ContractCostType = ContractCostType::MlDsa65DecodeVerifyingKey;
    const DECODE_SIGNATURE: ContractCostType = ContractCostType::MlDsa65DecodeSignature;
    const VERIFY: ContractCostType = ContractCostType::VerifyMlDsa65Sig;
}

impl MlDsaCostTypes for MlDsa87 {
    const DECODE_VERIFYING_KEY: ContractCostType = ContractCostType::MlDsa87DecodeVerifyingKey;
    const DECODE_SIGNATURE: ContractCostType = ContractCostType::MlDsa87DecodeSignature;
    const VERIFY: ContractCostType = ContractCostType::VerifyMlDsa87Sig;
}

impl Host {
    pub(crate) fn ml_dsa_decode_verifying_key<P: MlDsaParams>(
        &self,
        bytes: &[u8],
        ty: &ContractCostType,
    ) -> Result<VerifyingKey<P>, HostError> {
        let enc: &EncodedVerifyingKey<P> = bytes.try_into().map_err(|_| {
            err!(
                self,
                (ScErrorType::Object, ScErrorCode::UnexpectedSize),
                "ML-DSA verifying key has wrong length",
                bytes.len(),
                core::mem::size_of::<EncodedVerifyingKey<P>>()
            )
        })?;
        self.charge_budget(*ty, None)?;
        Ok(VerifyingKey::<P>::decode(enc))
    }

    pub(crate) fn ml_dsa_decode_signature<P: MlDsaParams>(
        &self,
        bytes: &[u8],
        ty: &ContractCostType,
    ) -> Result<Signature<P>, HostError> {
        let enc: &EncodedSignature<P> = bytes.try_into().map_err(|_| {
            err!(
                self,
                (ScErrorType::Object, ScErrorCode::UnexpectedSize),
                "ML-DSA signature has wrong length",
                bytes.len(),
                core::mem::size_of::<EncodedSignature<P>>()
            )
        })?;
        self.charge_budget(*ty, None)?;
        Signature::<P>::decode(enc).ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "malformed ML-DSA signature",
                &[],
            )
        })
    }

    pub(crate) fn ml_dsa_verify_sig_internal<P: MlDsaParams>(
        &self,
        vk: &VerifyingKey<P>,
        msg: &[u8],
        context: &[u8],
        sig: &Signature<P>,
        ty: &ContractCostType,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("ml-dsa verify");
        self.charge_budget(
            *ty,
            Some((msg.len() as u64).saturating_add(context.len() as u64)),
        )?;
        if vk.verify_with_context(msg, context, sig) {
            Ok(())
        } else {
            Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "failed ML-DSA verification",
                &[],
            ))
        }
    }

    pub(crate) fn verify_sig_ml_dsa<P: MlDsaParams + MlDsaCostTypes>(
        &self,
        public_key: BytesObject,
        msg: BytesObject,
        signature: BytesObject,
        context: BytesObject,
    ) -> Result<(), HostError> {
        self.visit_obj(public_key, |pk: &ScBytes| {
            self.visit_obj(signature, |sg: &ScBytes| {
                self.visit_obj(context, |ctx: &ScBytes| {
                    self.visit_obj(msg, |m: &ScBytes| {
                        // Every length is checked before any expensive work, so
                        // an over-long context cannot be paid for with a key
                        // expansion.
                        if ctx.len() > ML_DSA_MAX_CONTEXT_LEN {
                            return Err(err!(
                                self,
                                (ScErrorType::Object, ScErrorCode::UnexpectedSize),
                                "ML-DSA context exceeds maximum length",
                                ctx.len(),
                                ML_DSA_MAX_CONTEXT_LEN
                            ));
                        }
                        let vk = self.ml_dsa_decode_verifying_key::<P>(
                            pk.as_slice(),
                            &P::DECODE_VERIFYING_KEY,
                        )?;
                        let sig =
                            self.ml_dsa_decode_signature::<P>(sg.as_slice(), &P::DECODE_SIGNATURE)?;
                        self.ml_dsa_verify_sig_internal::<P>(
                            &vk,
                            m.as_slice(),
                            ctx.as_slice(),
                            &sig,
                            &P::VERIFY,
                        )
                    })
                })
            })
        })
    }
}
