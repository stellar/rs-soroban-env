use crate::{budget::CostType, cost_runner::CostRunner, xdr::ScVal};

pub struct ValDeserRun;

impl CostRunner for ValDeserRun {
    const COST_TYPE: CostType = CostType::ValDeser;

    type SampleType = Vec<u8>;

    type RecycledType = (Option<ScVal>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let sv = host.metered_from_xdr::<ScVal>(&sample).unwrap();
        (Some(sv), sample)
    }

    fn run_baseline_iter(
        _host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        (None, sample)
    }
}
