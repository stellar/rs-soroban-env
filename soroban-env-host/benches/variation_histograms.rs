// Run this with
// $ RUN_EXPERIMENT=1 cargo bench --features bench,next --bench variation_histograms -- --nocapture
mod common;
use common::*;
use rand::{rngs::StdRng, Rng, SeedableRng};
use soroban_env_host::{budget::MeteredCostComponent, cost_runner::CostRunner};

#[allow(unused)]
fn gen_random_input(lower: u64, upper: u64) -> u64 {
    let mut rng = StdRng::from_seed([0xff; 32]);
    rng.gen_range(lower..=upper)
}

struct LinearModelTables;
impl Benchmark for LinearModelTables {
    fn bench<HCM: HostCostMeasurement>(
    ) -> std::io::Result<(MeteredCostComponent, MeteredCostComponent)> {
        // the inputs will be ignored if the measurment is for a constant model
        let mut measurements = measure_cost_variation::<HCM>(100_000, || 0, || 0, false)?;
        measurements.check_range_against_baseline(&HCM::Runner::COST_TYPE)?;
        measurements.preprocess();
        measurements.report_histogram("cpu", |m| m.cpu_insns);
        measurements.report_histogram("mem", |m| m.mem_bytes);
        measurements.report_histogram("time [nsecs]", |m| m.time_nsecs);
        Ok(Default::default())
    }
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn main() -> std::io::Result<()> {
    let _params = if std::env::var("RUN_EXPERIMENT").is_err() {
        for_each_host_cost_measurement::<LinearModelTables>()?
    } else {
        for_each_experimental_cost_measurement::<LinearModelTables>()?
    };

    Ok(())
}
