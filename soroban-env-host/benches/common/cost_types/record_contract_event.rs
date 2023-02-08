use crate::common::HostCostMeasurement;
use soroban_env_host::xdr::{ScVal, ScVec};
use soroban_env_host::VecObject;
use soroban_env_host::{
    cost_runner::{RecordContractEventRun, RecordContractEventSample},
    Host, RawVal,
};

pub(crate) struct RecordContractEventMeasure;

impl HostCostMeasurement for RecordContractEventMeasure {
    type Runner = RecordContractEventRun;

    fn new_random_case(
        host: &Host,
        _rng: &mut rand::prelude::StdRng,
        input: u64,
    ) -> RecordContractEventSample {
        let topics: ScVec = vec![ScVal::U32(0), ScVal::U32(1), ScVal::U32(2), ScVal::U32(3)]
            .try_into()
            .unwrap();
        let topics = ScVal::Vec(Some(topics));
        let topics: VecObject = host.inject_val(&topics).unwrap().try_into().unwrap();
        let data: Vec<ScVal> = (0..input).map(|i| ScVal::U32(i as u32)).collect();
        let data: ScVec = data.try_into().unwrap();
        let data: ScVal = ScVal::Vec(Some(data));
        let data: RawVal = host.inject_val(&data).unwrap();

        let count = 4 + input;
        RecordContractEventSample {
            topics,
            data,
            count,
        }
    }
}
