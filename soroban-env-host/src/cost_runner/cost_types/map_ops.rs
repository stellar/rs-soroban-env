use crate::{budget::CostType, cost_runner::CostRunner, Host, MeteredOrdMap, RawVal};

type HostMap = MeteredOrdMap<RawVal, RawVal, Host>;

pub struct MapNewRun;

impl CostRunner for MapNewRun {
    const COST_TYPE: CostType = CostType::MapNew;

    type SampleType = ();

    type RecycledType = Option<HostMap>;

    fn run_iter(host: &Host, _iter: u64, _sample: Self::SampleType) -> Self::RecycledType {
        Some(HostMap::new(host).unwrap())
    }

    fn run_baseline_iter(
        _host: &Host,
        _iter: u64,
        _sample: Self::SampleType,
    ) -> Self::RecycledType {
        None
    }
}

pub struct MapEntryRun;

#[derive(Clone)]
pub struct MapEntrySample {
    pub map: HostMap,
    pub keys: Vec<RawVal>,
}

impl CostRunner for MapEntryRun {
    const COST_TYPE: CostType = CostType::MapEntry;

    type SampleType = MapEntrySample;

    type RecycledType = (Option<u64>, Self::SampleType);

    fn run_iter(host: &crate::Host, iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let v = sample
            .map
            .get(&sample.keys[iter as usize % sample.keys.len()], host)
            .unwrap()
            .unwrap()
            .get_payload();
        (Some(v), sample)
    }

    fn run_baseline_iter(_host: &Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        (None, sample)
    }
}
