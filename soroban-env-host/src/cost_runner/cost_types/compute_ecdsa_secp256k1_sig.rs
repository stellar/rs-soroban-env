use std::hint::black_box;

use k256::ecdsa::Signature;

use crate::{
    cost_runner::{CostRunner, CostType},
    xdr::ContractCostType::ComputeEcdsaSecp256k1Sig,
};

pub struct ComputeEcdsaSecp256k1SigRun;

impl CostRunner for ComputeEcdsaSecp256k1SigRun {
    const COST_TYPE: CostType = CostType::Contract(ComputeEcdsaSecp256k1Sig);

    type SampleType = Vec<u8>;

    type RecycledType = (Option<Signature>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let pk = black_box(
            host.secp256k1_signature_from_bytes(sample.as_slice())
                .expect("ecdsa secp256k1 signature"),
        );
        (Some(pk), sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(ComputeEcdsaSecp256k1Sig, None).unwrap());
        black_box((None, sample))
    }
}
