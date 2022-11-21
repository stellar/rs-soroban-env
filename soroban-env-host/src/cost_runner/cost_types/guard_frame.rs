use crate::{budget::CostType, cost_runner::CostRunner, host::Frame, xdr::Hash, Status, Symbol};

pub struct GuardFrameRun;

impl CostRunner for GuardFrameRun {
    const COST_TYPE: CostType = CostType::GuardFrame;

    type SampleType = (Hash, Symbol);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        host.with_frame(Frame::Token(sample.0, sample.1), || Ok(Status::OK.to_raw()))
            .unwrap();
    }
}
