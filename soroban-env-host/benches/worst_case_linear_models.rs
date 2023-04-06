// Run this with
// $ cargo bench --features vm,testutils --bench worst_case_linear_models -- --nocapture
// You can optionally pass in args listing the {`CostType`, `WasmInsnType`} combination to run with, e.g.
// $ cargo bench --features vm,testutils --bench worst_case_linear_models -- VecNew I64Rotr --nocapture
mod common;
use common::*;
use std::io::Write;
use tabwriter::{Alignment, TabWriter};

struct WorstCaseLinearModels;
impl Benchmark for WorstCaseLinearModels {
    fn bench<HCM: HostCostMeasurement>() -> std::io::Result<(FPCostModel, FPCostModel)> {
        let mut measurements = measure_worst_case_costs::<HCM>(0..20)?;

        measurements.preprocess();
        measurements.report_table();
        let cpu_model = measurements.fit_model_to_cpu();
        let mem_model = measurements.fit_model_to_mem();
        println!("cpu model params: {:?}", cpu_model);
        println!("mem model params: {:?}", mem_model);
        Ok((cpu_model, mem_model))
    }
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn main() -> std::io::Result<()> {
    env_logger::init();
    let params = for_each_host_cost_measurement::<WorstCaseLinearModels>()?;
    let params_wasm = for_each_wasm_insn_measurement::<WorstCaseLinearModels>()?;

    let mut tw = TabWriter::new(vec![])
        .padding(5)
        .alignment(Alignment::Right);

    writeln!(&mut tw, "").unwrap();
    writeln!(&mut tw, "").unwrap();
    writeln!(&mut tw, "cost_type\tcpu_model_const_param\tcpu_model_lin_param\tmem_model_const_param\tmem_model_lin_param").unwrap();
    for (ty, (cpu, mem)) in params
        .iter()
        .map(|(ty, (cpu, mem))| (ty, (cpu.params_as_u64(), mem.params_as_u64())))
    {
        writeln!(
            &mut tw,
            "{:?}\t{}\t{}\t{}\t{}",
            ty, cpu.0, cpu.1, mem.0, mem.1
        )
        .unwrap();
    }
    tw.flush().unwrap();

    writeln!(&mut tw, "").unwrap();
    writeln!(&mut tw, "").unwrap();
    writeln!(&mut tw, "wasm_insn_type\tcpu_model_const_param\tcpu_model_lin_param\tmem_model_const_param\tmem_model_lin_param").unwrap();
    for (ty, (cpu, mem)) in params_wasm
        .iter()
        .map(|(ty, (cpu, mem))| (ty, (cpu.params_as_u64(), mem.params_as_u64())))
    {
        writeln!(
            &mut tw,
            "{:?}\t{}\t{}\t{}\t{}",
            ty, cpu.0, cpu.1, mem.0, mem.1
        )
        .unwrap();
    }

    tw.flush().unwrap();
    eprintln!("{}", String::from_utf8(tw.into_inner().unwrap()).unwrap());

    Ok(())
}
