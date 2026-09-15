use crate::common::HostCostMeasurement;
use ml_dsa::{ExpandedSigningKey, MlDsaParams, Seed};
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{
    cost_runner::{
        MlDsa44DecodeSignatureRun, MlDsa44DecodeSignatureSample, MlDsa44DecodeVerifyingKeyRun,
        MlDsa44DecodeVerifyingKeySample, MlDsa65DecodeSignatureRun, MlDsa65DecodeSignatureSample,
        MlDsa65DecodeVerifyingKeyRun, MlDsa65DecodeVerifyingKeySample, MlDsa87DecodeSignatureRun,
        MlDsa87DecodeSignatureSample, MlDsa87DecodeVerifyingKeyRun,
        MlDsa87DecodeVerifyingKeySample, VerifyMlDsa44SigRun, VerifyMlDsa44SigSample,
        VerifyMlDsa65SigRun, VerifyMlDsa65SigSample, VerifyMlDsa87SigRun, VerifyMlDsa87SigSample,
    },
    xdr::ContractCostType::*,
    Host,
};

fn signing_key<P: MlDsaParams>(rng: &mut StdRng) -> ExpandedSigningKey<P> {
    let mut seed = [0u8; 32];
    rng.fill_bytes(&mut seed);
    ExpandedSigningKey::<P>::from_seed(&Seed::try_from(&seed[..]).unwrap())
}

macro_rules! impl_ml_dsa_measurements {
    ($params: ty,
     $decode_vk_measure: ident, $decode_vk_run: ident, $decode_vk_sample: ident, $decode_vk_cost: ident,
     $decode_sig_measure: ident, $decode_sig_run: ident, $decode_sig_sample: ident, $decode_sig_cost: ident,
     $verify_measure: ident, $verify_run: ident, $verify_sample: ident, $verify_cost: ident) => {
        pub(crate) struct $decode_vk_measure;
        impl HostCostMeasurement for $decode_vk_measure {
            type Runner = $decode_vk_run;

            fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> $decode_vk_sample {
                let sk = signing_key::<$params>(rng);
                $decode_vk_sample(sk.verifying_key().encode().to_vec(), $decode_vk_cost)
            }
        }

        pub(crate) struct $decode_sig_measure;
        impl HostCostMeasurement for $decode_sig_measure {
            type Runner = $decode_sig_run;

            fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> $decode_sig_sample {
                let sk = signing_key::<$params>(rng);
                let sig = sk.sign_deterministic(b"benchmark", &[]).unwrap();
                $decode_sig_sample(sig.encode().to_vec(), $decode_sig_cost)
            }
        }

        // The input is the signed message length; the context is left empty so
        // the swept input equals what the host charges (message + context).
        pub(crate) struct $verify_measure;
        impl HostCostMeasurement for $verify_measure {
            type Runner = $verify_run;
            const INPUT_BASE_SIZE: u64 = 0;

            fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> $verify_sample {
                let size = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
                let sk = signing_key::<$params>(rng);
                let msg: Vec<u8> = (0..size).map(|x| x as u8).collect();
                let sig = sk.sign_deterministic(msg.as_slice(), &[]).unwrap();
                $verify_sample(sk.verifying_key(), msg, Vec::new(), sig, $verify_cost)
            }
        }
    };
}

impl_ml_dsa_measurements!(
    ml_dsa::MlDsa44,
    MlDsa44DecodeVerifyingKeyMeasure,
    MlDsa44DecodeVerifyingKeyRun,
    MlDsa44DecodeVerifyingKeySample,
    MlDsa44DecodeVerifyingKey,
    MlDsa44DecodeSignatureMeasure,
    MlDsa44DecodeSignatureRun,
    MlDsa44DecodeSignatureSample,
    MlDsa44DecodeSignature,
    VerifyMlDsa44SigMeasure,
    VerifyMlDsa44SigRun,
    VerifyMlDsa44SigSample,
    VerifyMlDsa44Sig
);

impl_ml_dsa_measurements!(
    ml_dsa::MlDsa65,
    MlDsa65DecodeVerifyingKeyMeasure,
    MlDsa65DecodeVerifyingKeyRun,
    MlDsa65DecodeVerifyingKeySample,
    MlDsa65DecodeVerifyingKey,
    MlDsa65DecodeSignatureMeasure,
    MlDsa65DecodeSignatureRun,
    MlDsa65DecodeSignatureSample,
    MlDsa65DecodeSignature,
    VerifyMlDsa65SigMeasure,
    VerifyMlDsa65SigRun,
    VerifyMlDsa65SigSample,
    VerifyMlDsa65Sig
);

impl_ml_dsa_measurements!(
    ml_dsa::MlDsa87,
    MlDsa87DecodeVerifyingKeyMeasure,
    MlDsa87DecodeVerifyingKeyRun,
    MlDsa87DecodeVerifyingKeySample,
    MlDsa87DecodeVerifyingKey,
    MlDsa87DecodeSignatureMeasure,
    MlDsa87DecodeSignatureRun,
    MlDsa87DecodeSignatureSample,
    MlDsa87DecodeSignature,
    VerifyMlDsa87SigMeasure,
    VerifyMlDsa87SigRun,
    VerifyMlDsa87SigSample,
    VerifyMlDsa87Sig
);
