use ml_dsa::{MlDsa44, MlDsa65, MlDsa87, Signature, VerifyingKey};

use crate::{
    cost_runner::{CostRunner, CostType},
    impl_const_cost_runner_for_bls_deref_sample, impl_lin_cost_runner_for_bls_deref_sample,
    xdr::ContractCostType::{
        self, MlDsa44DecodeSignature, MlDsa44DecodeVerifyingKey, MlDsa65DecodeSignature,
        MlDsa65DecodeVerifyingKey, MlDsa87DecodeSignature, MlDsa87DecodeVerifyingKey,
        VerifyMlDsa44Sig, VerifyMlDsa65Sig, VerifyMlDsa87Sig,
    },
    Host,
};
use std::hint::black_box;

macro_rules! impl_ml_dsa_cost_runners {
    ($params: ty,
     $decode_vk_run: ident, $decode_vk_sample: ident, $decode_vk_cost: ident,
     $decode_sig_run: ident, $decode_sig_sample: ident, $decode_sig_cost: ident,
     $verify_run: ident, $verify_sample: ident, $verify_cost: ident) => {
        pub struct $decode_vk_run;
        pub struct $decode_sig_run;
        pub struct $verify_run;

        #[derive(Clone)]
        pub struct $decode_vk_sample(pub Vec<u8>, pub ContractCostType);
        #[derive(Clone)]
        pub struct $decode_sig_sample(pub Vec<u8>, pub ContractCostType);
        #[derive(Clone)]
        pub struct $verify_sample(
            pub VerifyingKey<$params>,
            pub Vec<u8>,
            pub Vec<u8>,
            pub Signature<$params>,
            pub ContractCostType,
        );

        impl_const_cost_runner_for_bls_deref_sample!(
            $decode_vk_run,
            $decode_vk_cost,
            ml_dsa_decode_verifying_key,
            $decode_vk_sample,
            VerifyingKey<$params>,
            bytes,
            ty
        );

        impl_const_cost_runner_for_bls_deref_sample!(
            $decode_sig_run,
            $decode_sig_cost,
            ml_dsa_decode_signature,
            $decode_sig_sample,
            Signature<$params>,
            bytes,
            ty
        );

        impl_lin_cost_runner_for_bls_deref_sample!(
            $verify_run,
            $verify_cost,
            ml_dsa_verify_sig_internal,
            $verify_sample,
            (),
            vk,
            msg,
            ctx,
            sig,
            ty
        );
    };
}

impl_ml_dsa_cost_runners!(
    MlDsa44,
    MlDsa44DecodeVerifyingKeyRun,
    MlDsa44DecodeVerifyingKeySample,
    MlDsa44DecodeVerifyingKey,
    MlDsa44DecodeSignatureRun,
    MlDsa44DecodeSignatureSample,
    MlDsa44DecodeSignature,
    VerifyMlDsa44SigRun,
    VerifyMlDsa44SigSample,
    VerifyMlDsa44Sig
);

impl_ml_dsa_cost_runners!(
    MlDsa65,
    MlDsa65DecodeVerifyingKeyRun,
    MlDsa65DecodeVerifyingKeySample,
    MlDsa65DecodeVerifyingKey,
    MlDsa65DecodeSignatureRun,
    MlDsa65DecodeSignatureSample,
    MlDsa65DecodeSignature,
    VerifyMlDsa65SigRun,
    VerifyMlDsa65SigSample,
    VerifyMlDsa65Sig
);

impl_ml_dsa_cost_runners!(
    MlDsa87,
    MlDsa87DecodeVerifyingKeyRun,
    MlDsa87DecodeVerifyingKeySample,
    MlDsa87DecodeVerifyingKey,
    MlDsa87DecodeSignatureRun,
    MlDsa87DecodeSignatureSample,
    MlDsa87DecodeSignature,
    VerifyMlDsa87SigRun,
    VerifyMlDsa87SigSample,
    VerifyMlDsa87Sig
);
