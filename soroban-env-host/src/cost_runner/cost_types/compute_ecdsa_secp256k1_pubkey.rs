use std::hint::black_box;

use k256::PublicKey;

use crate::{cost_runner::CostRunner, xdr::ContractCostType};

pub struct ComputeEcdsaSecp256k1PubKeyRun;

impl CostRunner for ComputeEcdsaSecp256k1PubKeyRun {
    const COST_TYPE: ContractCostType = ContractCostType::ComputeEcdsaSecp256k1Key;

    type SampleType = Vec<u8>;

    type RecycledType = (Option<PublicKey>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let pk = black_box(
            host.secp256k1_pub_key_from_bytes(sample.as_slice())
                .expect("ecdsa secp256k1 publickey"),
        );
        (Some(pk), sample)
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, None).unwrap());
        black_box((None, sample))
    }
}
