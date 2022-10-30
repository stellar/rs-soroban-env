use crate::{budget::CostType, cost_runner::CostRunner, EnvVal, Host, RawVal};

pub struct ImMapImmutEntryRun;

pub struct ImMapImmutEntrySample {
    pub map: im_rc::OrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>>,
    pub keys: Vec<EnvVal<Host, RawVal>>,
}

impl CostRunner for ImMapImmutEntryRun {
    const COST_TYPE: CostType = CostType::ImMapImmutEntry;
    type SampleType = ImMapImmutEntrySample;

    fn run_iter(_host: &crate::Host, iter: u64, sample: &mut Self::SampleType) -> Option<u64> {
        let _ = sample
            .map
            .get(&sample.keys[iter as usize % sample.keys.len()]);
        Some(sample.map.len() as u64)
    }
}
