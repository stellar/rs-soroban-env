use crate::{budget::CostType, cost_runner::CostRunner, events::HostEvent, xdr::ContractEvent};

pub struct RecordContractEventRun;

pub struct RecordContractEventSample {
    pub storage: Vec<HostEvent>,
    pub events: Vec<ContractEvent>,
}

impl CostRunner for RecordContractEventRun {
    const COST_TYPE: CostType = CostType::HostEventContract;
    type SampleType = RecordContractEventSample;

    fn run_iter(_host: &crate::Host, _iter: u64, sample: &mut Self::SampleType) -> Option<u64> {
        // TODO: this may panic
        sample
            .storage
            .push(HostEvent::Contract(sample.events.pop().unwrap()));
        Some(1)
    }
}
