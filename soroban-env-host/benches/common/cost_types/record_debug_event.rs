use crate::common::{util, HostCostMeasurement};
use soroban_env_host::{
    cost_runner::{CreateRecordDebugEventRun, CreateRecordDebugEventSample},
    Host,
};

pub(crate) struct CreateRecordDebugEventMeasure;

impl HostCostMeasurement for CreateRecordDebugEventMeasure {
    type Runner = CreateRecordDebugEventRun;

    fn new_random_case(
        _host: &Host,
        _rng: &mut rand::prelude::StdRng,
        input: u64,
    ) -> CreateRecordDebugEventSample {
        let args = util::to_rawval_u32(0..(input as u32)).collect();
        CreateRecordDebugEventSample {
            msg: "this is a debug msg",
            args,
        }
    }

    fn new_best_case(
        _host: &Host,
        _rng: &mut rand::prelude::StdRng,
    ) -> CreateRecordDebugEventSample {
        CreateRecordDebugEventSample {
            msg: "",
            args: Vec::default(),
        }
    }
}
