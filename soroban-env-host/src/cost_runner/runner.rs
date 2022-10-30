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
    type SampleType;

    /// Run a iteration of the `CostRunner`, called by `run` for 0..RUN_ITERATIONS.
    /// Optionally returns the cost input consumed by this iteration, only return `Some`
    /// if the host is not actually involved in the actual run.
    fn run_iter(host: &Host, iter: u64, sample: &mut Self::SampleType) -> Option<u64>;

    /// Run the `CostRunner`. This method is called under CPU-and-memory tracking
    /// machinery, so anything that happens during it will be considered part of
    /// the cost for running the HostMeasurement at the returned input. Will call
    /// `run_iter` with iter set to each number in 0..RUN_ITERATIONS.
    /// Returns the total input consumed by this run.
    fn run(host: &Host, sample: &mut Self::SampleType) -> u64 {
        let mut iter_input = None;
        for iter in 0..Self::RUN_ITERATIONS {
            iter_input = Self::run_iter(host, iter, sample);
        }
        match iter_input {
            Some(i) => i * Self::RUN_ITERATIONS,
            None => host.with_budget(|budget| budget.get_input(Self::COST_TYPE)),
        }
    }
}
