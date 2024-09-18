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

#[macro_export]
macro_rules! impl_const_cost_runner_for_bls_consume_sample {
    ($runner: ident, $cost: ident, $host_fn: ident, $sample: ident, $rt: ty, $($arg: ident),*) => {
        impl CostRunner for $runner {
            const COST_TYPE: CostType = CostType::Contract($cost);

            const RUN_ITERATIONS: u64 = 1;

            type SampleType = $sample;

            type RecycledType = (Option<$sample>, Option<$rt>);

            fn run_iter(host: &Host, _iter: u64, sample: $sample) -> Self::RecycledType {
                let $sample($( $arg ),*) = sample;
                let res = host.$host_fn($($arg),*).unwrap();
                black_box((None, Some(res)))
            }

            fn run_baseline_iter(
                host: &Host,
                _iter: u64,
                sample: $sample,
            ) -> Self::RecycledType {
                black_box(
                    host.charge_budget($cost, None)
                        .unwrap(),
                );
                black_box((Some(sample), None))
            }
        }
    };
}

#[macro_export]
macro_rules! impl_lin_cost_runner_for_bls_deref_sample {
    ($runner: ident, $cost: ident, $host_fn: ident, $sample: ident, $rt: ty, $($arg: ident),*) => {
        impl CostRunner for $runner {
            const COST_TYPE: CostType = CostType::Contract($cost);

            const RUN_ITERATIONS: u64 = 100;

            type SampleType = $sample;

            type RecycledType = ($sample, Option<$rt>);

            fn run_iter(host: &Host, _iter: u64, mut sample: $sample) -> Self::RecycledType {
                let $sample($( $arg ),*) = &mut sample;
                let res = host.$host_fn($($arg),*).unwrap();
                black_box((sample, Some(res)))
            }

            fn run_baseline_iter(
                host: &Host,
                _iter: u64,
                sample: $sample,
            ) -> Self::RecycledType {
                black_box(
                    host.charge_budget($cost, Some(1))
                        .unwrap(),
                );
                black_box((sample, None))
            }
        }
    };
}

#[macro_export]
macro_rules! impl_const_cost_runner_for_bls_deref_sample {
    ($runner: ident, $cost: ident, $host_fn: ident, $sample: ident, $rt: ty, $($arg: ident),*) => {
        impl CostRunner for $runner {
            const COST_TYPE: CostType = CostType::Contract($cost);

            const RUN_ITERATIONS: u64 = 1;

            type SampleType = $sample;

            type RecycledType = (Option<$sample>, Option<$rt>);

            fn run_iter(host: &Host, _iter: u64, mut sample: $sample) -> Self::RecycledType {
                let $sample($( $arg ),*) = &mut sample;
                let res = host.$host_fn($($arg),*).unwrap();
                black_box((Some(sample), Some(res)))
            }

            fn run_baseline_iter(
                host: &Host,
                _iter: u64,
                sample: $sample,
            ) -> Self::RecycledType {
                black_box(
                    host.charge_budget($cost, None)
                        .unwrap(),
                );
                black_box((Some(sample), None))
            }
        }
    };
}
