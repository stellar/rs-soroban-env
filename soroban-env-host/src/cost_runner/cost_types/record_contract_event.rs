use crate::{budget::CostType, cost_runner::CostRunner, events::HostEvent, xdr::ContractEvent};

pub struct RecordContractEventRun;

#[derive(Clone)]
pub struct RecordContractEventSample {
    pub storage: Vec<HostEvent>,
    pub events: Vec<ContractEvent>,
}

impl CostRunner for RecordContractEventRun {
    const COST_TYPE: CostType = CostType::HostEventContract;
    type SampleType = RecordContractEventSample;

    fn run_iter(_host: &crate::Host, _iter: u64, mut sample: Self::SampleType) {
        sample
            .storage
            .push(HostEvent::Contract(sample.events.pop().unwrap()));
    }

    fn get_total_input(_host: &crate::Host, _sample: &Self::SampleType) -> u64 {
        Self::RUN_ITERATIONS
    }
}
