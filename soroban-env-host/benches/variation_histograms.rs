// Run this with
// $ cargo bench --features vm,testutils --bench variation_histograms -- --nocapture
mod common;
use common::*;

struct LinearModelTables;
impl Benchmark for LinearModelTables {
    fn bench<HCM: HostCostMeasurement>() -> std::io::Result<()> {
        let mut measurements = measure_cost_variation::<HCM>(100)?;
        measurements.subtract_baseline();
        measurements.report_histogram("cpu", |m| m.cpu_insns);
        measurements.report_histogram("mem", |m| m.mem_bytes);
        Ok(())
    }
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn main() -> std::io::Result<()> {
    env_logger::init();
    for_each_host_cost_measurement::<LinearModelTables>()
}
