// Run this with
// $ cargo bench --features testutils --bench variation_histograms -- --nocapture
mod common;
use common::*;
use soroban_env_host::{budget::MeteredCostComponent, cost_runner::CostRunner};

struct LinearModelTables;
impl Benchmark for LinearModelTables {
    fn bench<HCM: HostCostMeasurement>(
    ) -> std::io::Result<(MeteredCostComponent, MeteredCostComponent)> {
        let mut measurements = measure_cost_variation::<HCM>(100, 1000, false, false)?;
        measurements.check_range_against_baseline(&HCM::Runner::COST_TYPE)?;
        measurements.preprocess();
        measurements.report_histogram("cpu", |m| m.cpu_insns);
        measurements.report_histogram("mem", |m| m.mem_bytes);
        Ok(Default::default())
    }
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn main() -> std::io::Result<()> {
    for_each_experimental_cost_measurement::<LinearModelTables>()?;
    Ok(())
}
