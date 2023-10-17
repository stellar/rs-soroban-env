use std::hint::black_box;

use crate::{cost_runner::CostRunner, xdr::ContractCostType, Host, MeteredOrdMap, Val};

type HostMap = MeteredOrdMap<Val, Val, Host>;

pub struct MapEntryRun;

#[derive(Clone)]
pub struct MapEntrySample {
    pub map: HostMap,
    pub keys: Vec<Val>,
}

impl CostRunner for MapEntryRun {
    const COST_TYPE: ContractCostType = ContractCostType::MapEntry;

    const RUN_ITERATIONS: u64 = 1000;

    type SampleType = MapEntrySample;

    type RecycledType = Self::SampleType;

    fn run_iter(host: &crate::Host, iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let _ = black_box(
            sample
                .map
                .get_at_index(iter as usize % sample.keys.len(), host)
                .unwrap(),
        );
        sample
    }

    fn run_baseline_iter(host: &Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        black_box(host.charge_budget(Self::COST_TYPE, None).unwrap());
        black_box(sample)
    }
}
