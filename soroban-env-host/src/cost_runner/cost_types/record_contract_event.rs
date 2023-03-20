use soroban_env_common::VecObject;

use crate::{budget::CostType, cost_runner::CostRunner, xdr::ContractEventType, RawVal};

pub struct RecordContractEventRun;

#[derive(Clone)]
pub struct RecordContractEventSample {
    pub topics: VecObject,
    pub data: RawVal,
    pub count: u64,
}

impl CostRunner for RecordContractEventRun {
    const COST_TYPE: CostType = CostType::HostEventContract;
    type SampleType = RecordContractEventSample;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        host.record_contract_event(ContractEventType::Contract, sample.topics, sample.data)
            .expect("contract event");
    }

    fn get_total_input(_host: &crate::Host, sample: &Self::SampleType) -> u64 {
        Self::RUN_ITERATIONS * sample.count
    }
}
