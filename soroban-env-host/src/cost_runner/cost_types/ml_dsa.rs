use crate::{
    cost_runner::{CostRunner, CostType},
    xdr::ContractCostType::{
        MlDsa44DecodeSignature, MlDsa44DecodeVerifyingKey, MlDsa65DecodeSignature,
        MlDsa65DecodeVerifyingKey, MlDsa87DecodeSignature, MlDsa87DecodeVerifyingKey,
        VerifyMlDsa44Sig, VerifyMlDsa65Sig, VerifyMlDsa87Sig,
    },
};
use ml_dsa::{MlDsa44, MlDsa65, MlDsa87, MlDsaParams, Signature, VerifyingKey};
use std::hint::black_box;

/// Sample for measuring verifying-key decoding (constant cost per variant):
/// the encoded verifying key bytes. Decoding includes the SHAKE-128 expansion
/// of the A_hat matrix and the NTT precompute, which dominate.
#[derive(Clone)]
pub struct MlDsaDecodeVerifyingKeySample {
    pub bytes: Vec<u8>,
}

/// Sample for measuring signature decoding (constant cost per variant): the
/// encoded signature bytes. Decoding unpacks and validates the hint and
/// response vectors.
#[derive(Clone)]
pub struct MlDsaDecodeSignatureSample {
    pub bytes: Vec<u8>,
}

/// Sample for measuring verification with a pre-decoded key and signature
/// (linear in the combined message and context length, per variant).
#[derive(Clone)]
pub struct MlDsaVerifySigSample<P: MlDsaParams> {
    pub vk: VerifyingKey<P>,
    pub msg: Vec<u8>,
    pub sig: Signature<P>,
    pub ctx: Vec<u8>,
}

macro_rules! impl_ml_dsa_runners {
    (
        $p:ty,
        $decode_vk_run:ident, $decode_vk_ct:ident,
        $decode_sig_run:ident, $decode_sig_ct:ident,
        $verify_run:ident, $verify_ct:ident
    ) => {
        pub struct $decode_vk_run;

        impl CostRunner for $decode_vk_run {
            const COST_TYPE: CostType = CostType::Contract($decode_vk_ct);
            // We want to capture the output variance w.r.t the random input,
            // thus we set `RUN_ITERATIONS` to 1.
            const RUN_ITERATIONS: u64 = 1;
            type SampleType = MlDsaDecodeVerifyingKeySample;
            type RecycledType = (Self::SampleType, Option<VerifyingKey<$p>>);

            fn run_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                let vk = host
                    .ml_dsa_verifying_key_from_slice::<$p>(&sample.bytes)
                    .unwrap();
                black_box((sample, Some(vk)))
            }

            fn run_baseline_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                black_box(host.charge_budget($decode_vk_ct, None).unwrap());
                black_box((sample, None))
            }
        }

        pub struct $decode_sig_run;

        impl CostRunner for $decode_sig_run {
            const COST_TYPE: CostType = CostType::Contract($decode_sig_ct);
            const RUN_ITERATIONS: u64 = 1;
            type SampleType = MlDsaDecodeSignatureSample;
            type RecycledType = (Self::SampleType, Option<Signature<$p>>);

            fn run_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                let sig = host
                    .ml_dsa_signature_from_slice::<$p>(&sample.bytes)
                    .unwrap();
                black_box((sample, Some(sig)))
            }

            fn run_baseline_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                black_box(host.charge_budget($decode_sig_ct, None).unwrap());
                black_box((sample, None))
            }
        }

        pub struct $verify_run;

        impl CostRunner for $verify_run {
            const COST_TYPE: CostType = CostType::Contract($verify_ct);
            const RUN_ITERATIONS: u64 = 1;
            type SampleType = MlDsaVerifySigSample<$p>;
            type RecycledType = Self::SampleType;

            fn run_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                black_box(
                    host.ml_dsa_verify_with_context::<$p>(
                        &sample.vk,
                        &sample.msg,
                        &sample.ctx,
                        &sample.sig,
                    )
                    .unwrap(),
                );
                black_box(sample)
            }

            fn run_baseline_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                black_box(
                    host.charge_budget(
                        $verify_ct,
                        Some((sample.msg.len() + sample.ctx.len()) as u64),
                    )
                    .unwrap(),
                );
                black_box(sample)
            }
        }
    };
}

impl_ml_dsa_runners!(
    MlDsa44,
    MlDsa44DecodeVerifyingKeyRun,
    MlDsa44DecodeVerifyingKey,
    MlDsa44DecodeSignatureRun,
    MlDsa44DecodeSignature,
    VerifyMlDsa44SigRun,
    VerifyMlDsa44Sig
);
impl_ml_dsa_runners!(
    MlDsa65,
    MlDsa65DecodeVerifyingKeyRun,
    MlDsa65DecodeVerifyingKey,
    MlDsa65DecodeSignatureRun,
    MlDsa65DecodeSignature,
    VerifyMlDsa65SigRun,
    VerifyMlDsa65Sig
);
impl_ml_dsa_runners!(
    MlDsa87,
    MlDsa87DecodeVerifyingKeyRun,
    MlDsa87DecodeVerifyingKey,
    MlDsa87DecodeSignatureRun,
    MlDsa87DecodeSignature,
    VerifyMlDsa87SigRun,
    VerifyMlDsa87Sig
);
