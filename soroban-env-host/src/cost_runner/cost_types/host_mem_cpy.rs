use crate::{budget::CostType, cost_runner::CostRunner};

pub struct HostMemCpyRun;

impl CostRunner for HostMemCpyRun {
    const COST_TYPE: CostType = CostType::HostMemCpy;

    type SampleType = (Vec<u8>, Vec<u8>);

    type RecycledType = Self::SampleType;

    fn run_iter(host: &crate::Host, _iter: u64, mut sample: Self::SampleType) -> Self::SampleType {
        host.mem_copy_from_slice(sample.0.as_slice(), sample.1.as_mut_slice())
            .unwrap();
        sample
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        sample
    }
}
