use crate::{budget::CostType, cost_runner::CostRunner, EnvVal, Host, MeteredOrdMap, RawVal};

type HostMap = MeteredOrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>>;

pub struct ImMapNewRun;
impl CostRunner for ImMapNewRun {
    const COST_TYPE: CostType = CostType::ImMapNew;
    type SampleType = ();

    fn run_iter(host: &Host, _iter: u64, _sample: Self::SampleType) {
        HostMap::new(host.budget_cloned()).unwrap();
    }
}

pub struct ImMapImmutEntryRun;
#[derive(Clone)]
pub struct ImMapImmutEntrySample {
    pub map: HostMap,
    pub keys: Vec<EnvVal<Host, RawVal>>,
}
impl CostRunner for ImMapImmutEntryRun {
    const COST_TYPE: CostType = CostType::ImMapImmutEntry;
    type SampleType = ImMapImmutEntrySample;

    fn run_iter(_host: &crate::Host, iter: u64, sample: Self::SampleType) {
        let _ = sample
            .map
            .get(&sample.keys[iter as usize % sample.keys.len()])
            .unwrap();
    }
}

pub struct ImMapMutEntryRun;
#[derive(Clone)]
pub struct ImMapMutEntrySample {
    pub im: ImMapImmutEntrySample,
    pub second_map_ref: MeteredOrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>>,
}
impl CostRunner for ImMapMutEntryRun {
    const COST_TYPE: CostType = CostType::ImMapMutEntry;
    type SampleType = ImMapMutEntrySample;

    fn run_iter(_host: &crate::Host, iter: u64, mut sample: Self::SampleType) {
        let _ = sample
            .im
            .map
            .get_mut(&sample.im.keys[iter as usize % sample.im.keys.len()])
            .unwrap();
    }
}
