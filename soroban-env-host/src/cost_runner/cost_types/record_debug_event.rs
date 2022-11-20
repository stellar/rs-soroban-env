use crate::{budget::CostType, cost_runner::CostRunner, events::DebugEvent, EnvVal, Host, RawVal};

pub struct CreateRecordDebugEventRun;

#[derive(Clone)]
pub struct CreateRecordDebugEventSample {
    pub msg: &'static str,
    pub args: Vec<EnvVal<Host, RawVal>>,
}

impl CostRunner for CreateRecordDebugEventRun {
    const COST_TYPE: CostType = CostType::HostEventDebug;
    type SampleType = CreateRecordDebugEventSample;

    fn run_iter(host: &Host, _iter: u64, sample: Self::SampleType) {
        let mut de = DebugEvent::new().msg(sample.msg);
        for arg in sample.args {
            de = de.arg(arg.to_raw());
        }
        host.record_debug_event(de).unwrap();
    }
}
