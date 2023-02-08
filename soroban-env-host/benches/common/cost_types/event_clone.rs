use crate::common::HostCostMeasurement;
use soroban_env_host::{
    cost_runner::EventCloneRun,
    events::{DebugEvent, InternalContractEvent, InternalEvent},
    xdr::{ContractEventType, ScObject, ScVal, ScVec},
    Host, Object, RawVal,
};

pub(crate) struct EventCloneMeasure;

impl EventCloneMeasure {
    fn new_debug_event(input: u64) -> InternalEvent {
        let mut de = DebugEvent::new().msg("this is a sample debug message");
        let len = input as u32 * 100;
        for i in 0..len {
            de = de.arg(RawVal::from(i));
        }
        InternalEvent::Debug(de)
    }

    fn new_contract_event(host: &Host, input: u64) -> InternalEvent {
        let id: ScVal = ScVal::Object(Some(ScObject::Bytes([0; 32].try_into().unwrap())));
        let id: Object = host.inject_val(&id).unwrap().try_into().unwrap();

        let topics: ScVec = vec![ScVal::U32(0), ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
            .try_into()
            .unwrap();
        let topics = ScVal::Object(Some(ScObject::Vec(topics)));
        let topics: Object = host.inject_val(&topics).unwrap().try_into().unwrap();
        let data: Vec<ScVal> = (0..input).map(|i| ScVal::U32(i as u32)).collect();
        let data: ScVec = data.try_into().unwrap();
        let data: ScVal = ScVal::Object(Some(ScObject::Vec(data)));
        let data: RawVal = host.inject_val(&data).unwrap();

        InternalEvent::Contract(InternalContractEvent {
            type_: ContractEventType::Contract,
            contract_id: Some(id),
            topics,
            data,
        })
    }
}

impl HostCostMeasurement for EventCloneMeasure {
    type Runner = EventCloneRun;

    fn new_random_case(host: &Host, _rng: &mut rand::prelude::StdRng, input: u64) -> InternalEvent {
        // Self::new_debug_event(input)
        Self::new_contract_event(host, input)
    }
}
