use super::CostType;
use crate::{
    budget::{AsBudget, CostTracker},
    xdr::ContractCostType,
    Host,
};
use std::hint::black_box;

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

    /// Data type to be recycled, this may include (but not limited to) the returned value and
    /// unused samples. The main reason for recycling are 1. avoid unwanted memory deallocation
    /// being counted as part of the run cost 2. prevent the optimizer from performing optimization
    /// around unused value, making the bench deviate from the real case.
    type RecycledType;

    /// Run an baseline iterations, to get an estimation of the benchmark overhead
    /// (`Overhead_b` in eq.[2], see [`HostCostMeasurement`]).
    fn run_baseline_iter(_host: &Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType;

    /// Run a iteration of the `CostRunner`, called by `run` for 0..RUN_ITERATIONS.
    /// Execution under `run_iter` is what's actually being measured by the bench
    /// machineary. Need to ensure as much as possible `run_iter` only calls essential
    /// host routines that go into the measurement. Any input setup needs to be done
    /// beforehand and passed in via `sample`.
    fn run_iter(host: &Host, iter: u64, sample: Self::SampleType) -> Self::RecycledType;

    /// Make sure `recycled` has been initialized with sufficient capacity needed to
    /// store all the recycled value such that no allocation is triggered that will
    /// polute the measurements.
    fn run_baseline(
        host: &Host,
        samples: Vec<Self::SampleType>,
        recycled: &mut Vec<Self::RecycledType>,
    ) {
        for (i, sample) in samples.into_iter().enumerate() {
            // use `black_box` to prevent unwanted optimization
            black_box(recycled.push(Self::run_baseline_iter(host, i as u64, sample)))
        }
    }

    /// Run the `CostRunner`. This method is called under CPU-and-memory tracking
    /// machinery, so anything that happens during it will be considered part of
    /// the cost for running the HostMeasurement at the returned input. Will call
    /// `run_iter` with iter set to each number in 0..RUN_ITERATIONS.
    /// Make sure `recycled` has been initialized with sufficient capacity to store
    /// all the recycled value such that no allocation is triggered that will polute
    /// the measurements.
    fn run(host: &Host, samples: Vec<Self::SampleType>, recycled: &mut Vec<Self::RecycledType>) {
        for (i, sample) in samples.into_iter().enumerate() {
            // use `black_box` to prevent unwanted optimization
            black_box(recycled.push(Self::run_iter(host, i as u64, sample)))
        }
    }

    /// Get the (iterations, inputs) tracker from this run. Default to asking the host.
    /// May be overridden if host is not actually involved in the actual run. However,
    /// if overridden, there is a risk of the computed input being diverged from the
    /// actual input from the host's perspective. So use it carefully. This should be
    /// after the `run`, outside of the CPU-and-memory tracking machineary.
    fn get_tracker(host: &Host) -> CostTracker {
        match Self::COST_TYPE {
            CostType::Contract(ct) => host.as_budget().get_tracker(ct).unwrap(),
            CostType::Experimental(_) => {
                panic!("experimental cost type cannot be tracked by the host")
            }
            CostType::Wasm(_) => host
                .as_budget()
                .get_tracker(ContractCostType::WasmInsnExec)
                .unwrap(),
        }
    }
}
