use crate::{budget::CostType, Host};
/// `CostRunner` is an interface to running a host cost entity of a `CostType` (usually a block of
/// WASM bytecode or a host function), given a sample of `SampleType`.
pub trait CostRunner: Sized {
    /// The type of cost we're measuring.
    const COST_TYPE: CostType;

    /// Number of iterations to run, used to divide the resulting measured values.
    /// Defaults to 100 to average out the measurement noises for fast-running cases.
    /// If you find your measurements are finishing too slow, reduce this number.
    const RUN_ITERATIONS: u64 = 100;

    /// Data type of the sample running with.
    type SampleType: Clone;

    /// Run a iteration of the `CostRunner`, called by `run` for 0..RUN_ITERATIONS.
    fn run_iter(host: &Host, iter: u64, sample: Self::SampleType);

    /// Run the `CostRunner`. This method is called under CPU-and-memory tracking
    /// machinery, so anything that happens during it will be considered part of
    /// the cost for running the HostMeasurement at the returned input. Will call
    /// `run_iter` with iter set to each number in 0..RUN_ITERATIONS.
    fn run(host: &Host, samples: Vec<Self::SampleType>) {
        for (iter, sample) in samples.into_iter().enumerate() {
            Self::run_iter(host, iter as u64, sample)
        }
    }

    /// Get the total input from this run. Default to asking the host. May be overridden
    /// if host is not actually involved in the actual run. However, if overridden, there is
    /// a risk of the computed input being diverged from the actual input from the host's
    /// perspective. So use it carefully. This should be after the `run`, outside of the
    /// CPU-and-memory tracking machineary.
    fn get_total_input(host: &Host, _sample: &Self::SampleType) -> u64 {
        host.0.budget.get_input(Self::COST_TYPE)
    }
}
