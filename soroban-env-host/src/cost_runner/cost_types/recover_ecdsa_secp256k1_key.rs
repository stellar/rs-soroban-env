use std::hint::black_box;

use crate::{
    cost_runner::{CostRunner, CostType},
    xdr::{ContractCostType::RecoverEcdsaSecp256k1Key, Hash},
};
use k256::ecdsa::{RecoveryId, Signature};

pub struct RecoverEcdsaSecp256k1KeyRun;

#[derive(Clone)]
pub struct RecoverEcdsaSecp256k1KeySample {
    pub hash: Hash,
    pub sig: Signature,
    pub rid: RecoveryId,
}

impl CostRunner for RecoverEcdsaSecp256k1KeyRun {
    const COST_TYPE: CostType = CostType::Contract(RecoverEcdsaSecp256k1Key);

    type SampleType = RecoverEcdsaSecp256k1KeySample;

    type RecycledType = Self::SampleType;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        black_box(
            host.recover_key_ecdsa_secp256k1_internal(&sample.hash, &sample.sig, sample.rid)
                .expect("recover ecdsa secp256k1 key"),
        );
        sample
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(RecoverEcdsaSecp256k1Key, None).unwrap());
        black_box(sample)
    }
}
