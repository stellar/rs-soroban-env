use crate::common::HostCostMeasurement;
use soroban_env_host::cost_runner::CostRunner;
use soroban_env_host::xdr::{
    ContractEvent, ContractEventBody, ContractEventType, ContractEventV0, ExtensionPoint, Hash,
    ScMap, ScMapEntry, ScObject::Map, ScVal,
};
use soroban_env_host::{
    cost_runner::{RecordContractEventRun, RecordContractEventSample},
    events::HostEvent,
    Host,
};

pub(crate) struct RecordContractEventMeasure;

impl HostCostMeasurement for RecordContractEventMeasure {
    type Runner = RecordContractEventRun;

    fn new_random_case(
        host: &Host,
        _rng: &mut rand::prelude::StdRng,
        input: u64,
    ) -> RecordContractEventSample {
        let event = ContractEvent {
            ext: ExtensionPoint::V0,
            contract_id: Some(Hash([0; 32])),
            type_: ContractEventType::Contract,
            body: ContractEventBody::V0(ContractEventV0 {
                topics: host
                    .map_err(vec![ScVal::U32(0), ScVal::U32(1)].try_into())
                    .unwrap(),
                data: ScVal::Object(Some(Map(host
                    .map_err(ScMap::try_from(vec![ScMapEntry {
                        key: ScVal::U32(1),
                        val: ScVal::U32(2),
                    }]))
                    .unwrap()))),
            }),
        };
        let mut storage = Vec::new();
        let mut events = Vec::new();
        for _iter in 0..input {
            storage.push(HostEvent::Contract(event.clone()));
        }
        for _iter in 0..<Self::Runner as CostRunner>::RUN_ITERATIONS {
            events.push(event.clone())
        }

        RecordContractEventSample { storage, events }
    }
}
