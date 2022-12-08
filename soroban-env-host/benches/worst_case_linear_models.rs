// Run this with
// $ cargo bench --features vm --bench worst_case_linear_models -- --nocapture
mod common;
use common::*;

struct WorstCaseLinearModels;
impl Benchmark for WorstCaseLinearModels {
    fn bench<HCM: HostCostMeasurement>() -> std::io::Result<()> {
        let measurements = measure_worst_case_costs::<HCM>(0..20)?;

        // TODO: decide what to do about baselines
        // measurements.subtract_baseline();

        measurements.report_table();

        if std::env::var("FIT_MODELS").is_ok() {
            measurements.fit_model_to_cpu();
            measurements.fit_model_to_mem();
        }
        Ok(())
    }
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn main() -> std::io::Result<()> {
    env_logger::init();
    for_each_host_cost_measurement::<WorstCaseLinearModels>()?;
    for_each_wasm_insn_measurement::<WorstCaseLinearModels>()?;
    Ok(())
}
