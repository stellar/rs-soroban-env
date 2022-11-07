use super::im_map_immut_entry::ImMapImmutEntrySample;
use crate::{budget::CostType, cost_runner::CostRunner, EnvVal, Host, RawVal};

pub struct ImMapMutEntryRun;

pub struct ImMapMutEntrySample {
    pub im: ImMapImmutEntrySample,
    pub second_map_ref: im_rc::OrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>>,
}

impl CostRunner for ImMapMutEntryRun {
    const COST_TYPE: CostType = CostType::ImMapMutEntry;
    type SampleType = ImMapMutEntrySample;

    fn run_iter(_host: &crate::Host, iter: u64, sample: &mut Self::SampleType) {
        let _ = sample
            .im
            .map
            .get_mut(&sample.im.keys[iter as usize % sample.im.keys.len()]);
    }

    fn get_total_input(_host: &Host, sample: &Self::SampleType) -> u64 {
        (sample.im.map.len() as u64) * Self::RUN_ITERATIONS
    }
}
