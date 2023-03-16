use crate::{budget::CostType, cost_runner::CostRunner, Host, MeteredOrdMap, RawVal};

type HostMap = MeteredOrdMap<RawVal, RawVal, Host>;

pub struct ImMapNewRun;
impl CostRunner for ImMapNewRun {
    const COST_TYPE: CostType = CostType::MapNew;
    type SampleType = ();

    fn run_iter(host: &Host, _iter: u64, _sample: Self::SampleType) {
        HostMap::new(host).unwrap();
    }
}

pub struct ImMapEntryRun;
#[derive(Clone)]
pub struct ImMapEntrySample {
    pub map: HostMap,
    pub keys: Vec<RawVal>,
}
impl CostRunner for ImMapEntryRun {
    const COST_TYPE: CostType = CostType::MapEntry;
    type SampleType = ImMapEntrySample;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        for k in &sample.keys {
            sample.map.get(k, host).unwrap();
        }
        // `forget` avoids deallocation of sample which artificially inflates the cost
        std::mem::forget(sample)
    }
}
