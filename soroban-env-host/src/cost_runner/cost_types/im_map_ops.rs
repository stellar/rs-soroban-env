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

pub struct ImMapImmutEntryRun;
#[derive(Clone)]
pub struct ImMapImmutEntrySample {
    pub map: HostMap,
    pub keys: Vec<RawVal>,
}
impl CostRunner for ImMapImmutEntryRun {
    const COST_TYPE: CostType = CostType::MapEntry;
    type SampleType = ImMapImmutEntrySample;

    fn run_iter(host: &crate::Host, iter: u64, sample: Self::SampleType) {
        let _ = sample
            .map
            .get(&sample.keys[iter as usize % sample.keys.len()], host)
            .unwrap();
    }
}
