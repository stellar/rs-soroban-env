// Run this with
// $ cargo bench --features bench --bench worst_case_linear_models -- --nocapture
// You can optionally pass in args listing the {`ContractCostType`, `WasmInsnType`} combination to run with, e.g.
// $ cargo bench --features bench --bench worst_case_linear_models -- MemCpy I64Rotr --nocapture
// To run the experimental cost types: $ RUN_EXPERIMENT=1 cargo bench ...
mod common;
use common::*;
use soroban_env_host::{
    cost_runner::{CostRunner, CostType, WasmInsnType},
    xdr::ContractCostType,
};
use std::{collections::BTreeMap, fmt::Display, io::Write};
use tabwriter::{Alignment, TabWriter};

struct WorstCaseLinearModels;
impl Benchmark for WorstCaseLinearModels {
    fn bench<HCM: HostCostMeasurement>() -> std::io::Result<(FPCostModel, FPCostModel)> {
        let floor = std::env::var("FLOOR").ok().map(|v| v.parse::<u64>().ok()).flatten().unwrap_or(1);
        let range = std::env::var("RANGE").ok().map(|v| v.parse::<u64>().ok()).flatten().unwrap_or(20);
        let mut measurements = measure_worst_case_costs::<HCM>(floor..range)?;
        measurements.check_range_against_baseline(&HCM::Runner::COST_TYPE)?;
        measurements.preprocess();
        measurements.report_table();
        let cpu_model = measurements.fit_model_to_cpu();
        let mem_model = measurements.fit_model_to_mem();
        println!("cpu model params: {:?}", cpu_model);
        println!("mem model params: {:?}", mem_model);
        Ok((cpu_model, mem_model))
    }
}

fn write_cost_params_table<T: Display>(
    tw: &mut TabWriter<Vec<u8>>,
    params: &BTreeMap<T, (FPCostModel, FPCostModel)>,
) -> std::io::Result<()> {
    writeln!(tw, "").unwrap();
    writeln!(tw, "").unwrap();
    writeln!(tw, "cost_type\tcpu_model_const_param\tcpu_model_lin_param\tmem_model_const_param\tmem_model_lin_param").unwrap();
    for (ty, (cpu, mem)) in params
        .iter()
        .map(|(ty, (cpu, mem))| (ty, (cpu.params_as_u64(), mem.params_as_u64())))
    {
        writeln!(tw, "{}\t{}\t{}\t{}\t{}", ty, cpu.0, cpu.1, mem.0, mem.1).unwrap();
    }
    tw.flush()
}

fn write_budget_params_code(
    params: &BTreeMap<CostType, (FPCostModel, FPCostModel)>,
    wasm_tier_cost: &BTreeMap<WasmInsnTier, f64>,
) {
    println!("");
    println!("");

    let base_cpu_per_fuel = wasm_tier_cost[&WasmInsnTier::BASE] as u64;
    let entity_cpu_per_fuel = wasm_tier_cost[&WasmInsnTier::ENTITY] as u64;
    let load_cpu_per_fuel = wasm_tier_cost[&WasmInsnTier::LOAD] as u64;
    let store_cpu_per_fuel = wasm_tier_cost[&WasmInsnTier::STORE] as u64;
    let call_cpu_per_fuel = wasm_tier_cost[&WasmInsnTier::CALL] as u64;
    println!(
        "
        // This is the host cpu insn cost per wasm \"fuel\". Every \"base\" wasm
        // instruction costs 1 fuel (by default), and some particular types of
        // instructions may cost additional amount of fuel based on
        // wasmi's config setting. \n
        ContractCostType::{:?} => {{ cpu.const_term = {}; cpu.lin_term = ScaledU64({}); }}",
        ContractCostType::WasmInsnExec,
        base_cpu_per_fuel,
        0
    );

    for (ty, (cpu, _)) in params
        .iter()
        .map(|(ty, (cpu, mem))| (ty, (cpu.params_as_u64(), mem.params_as_u64())))
    {
        if let CostType::Contract(ty) = ty {
            println!(
                "ContractCostType::{:?} => {{ cpu.const_term = {}; cpu.lin_term = ScaledU64({}); }}",
                ty, cpu.0, cpu.1
            );
        }
    }
    println!("");
    println!("");

    println!(
        "
        // This type is designated to the cpu cost. By definition, the memory cost\n
        // of a (cpu) fuel is zero.\n
        ContractCostType::{:?} => {{ mem.const_term = {}; mem.lin_term = ScaledU64({}); }}",
        ContractCostType::WasmInsnExec,
        0,
        0
    );
    for (ty, (_, mem)) in params
        .iter()
        .map(|(ty, (cpu, mem))| (ty, (cpu.params_as_u64(), mem.params_as_u64())))
    {
        if let CostType::Contract(ty) = ty {
            println!(
                "ContractCostType::{:?} => {{ mem.const_term = {}; mem.lin_term = ScaledU64({}); }}",
                ty, mem.0, mem.1
            );
        }
    }

    println!("");
    println!("");
    println!(
        "
        FuelConfig {{base: {}, entity: {}, load: {}, store: {}, call: {}}}",
        1,
        (entity_cpu_per_fuel / base_cpu_per_fuel).max(1),
        (load_cpu_per_fuel / base_cpu_per_fuel).max(1),
        (store_cpu_per_fuel / base_cpu_per_fuel).max(1),
        (call_cpu_per_fuel / base_cpu_per_fuel).max(1)
    )
}

fn extract_tier(
    params_wasm: &BTreeMap<CostType, (FPCostModel, FPCostModel)>,
    insn_tier: &[WasmInsnType],
) -> (BTreeMap<WasmInsnType, (FPCostModel, FPCostModel)>, f64) {
    let mut params_tier: BTreeMap<WasmInsnType, (FPCostModel, FPCostModel)> = BTreeMap::new();
    for ty in insn_tier {
        if let Some(res) = params_wasm.get(&CostType::Wasm(*ty)) {
            params_tier.insert(ty.clone(), res.clone());
        }
    }

    let cpu_per_fuel: Vec<f64> = params_tier
        .iter()
        .map(|(_, (cpu, _))| cpu.const_param)
        .collect();
    let ave_cpu_per_fuel = cpu_per_fuel.iter().sum::<f64>() / cpu_per_fuel.len() as f64;
    (params_tier, ave_cpu_per_fuel)
}

fn process_tier(
    tier: WasmInsnTier,
    params_wasm: &BTreeMap<CostType, (FPCostModel, FPCostModel)>,
    insn_tier: &[WasmInsnType],
) -> f64 {
    println!("\n");
    println!("\n{:=<100}", "");
    println!("\"{:?}\" tier", tier);

    let (params_tier, ave_cpu_per_fuel) = extract_tier(params_wasm, insn_tier);

    let mut tw = TabWriter::new(vec![])
        .padding(5)
        .alignment(Alignment::Right);
    write_cost_params_table::<WasmInsnType>(&mut tw, &params_tier).unwrap();
    eprintln!("{}", String::from_utf8(tw.into_inner().unwrap()).unwrap());
    println!(
        "average cpu insns per fuel for \"{:?}\" tier: {}",
        tier, ave_cpu_per_fuel
    );
    println!("{:=<100}\n", "");
    ave_cpu_per_fuel
}

fn extract_wasmi_fuel_costs(
    params_wasm: &BTreeMap<CostType, (FPCostModel, FPCostModel)>,
) -> BTreeMap<WasmInsnTier, f64> {
    let base_cost = process_tier(WasmInsnTier::BASE, params_wasm, &WASM_INSN_BASE);
    let entity_cost = process_tier(WasmInsnTier::ENTITY, params_wasm, &WASM_INSN_ENTITY);
    let load_cost = process_tier(WasmInsnTier::LOAD, params_wasm, &WASM_INSN_LOAD);
    let store_cost = process_tier(WasmInsnTier::STORE, params_wasm, &WASM_INSN_STORE);
    let call_cost = process_tier(WasmInsnTier::CALL, params_wasm, &WASM_INSN_CALL);
    let mut res: BTreeMap<WasmInsnTier, f64> = BTreeMap::new();
    res.insert(WasmInsnTier::BASE, base_cost);
    res.insert(WasmInsnTier::ENTITY, entity_cost);
    res.insert(WasmInsnTier::LOAD, load_cost);
    res.insert(WasmInsnTier::STORE, store_cost);
    res.insert(WasmInsnTier::CALL, call_cost);
    res
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn main() -> std::io::Result<()> {
    let params = if std::env::var("RUN_EXPERIMENT").is_err() {
        for_each_host_cost_measurement::<WorstCaseLinearModels>()?
    } else {
        for_each_experimental_cost_measurement::<WorstCaseLinearModels>()?
    };
    let params_wasm = for_each_wasm_insn_measurement::<WorstCaseLinearModels>()?;

    let mut tw = TabWriter::new(vec![])
        .padding(5)
        .alignment(Alignment::Right);
    write_cost_params_table::<CostType>(&mut tw, &params)?;
    eprintln!("{}", String::from_utf8(tw.into_inner().unwrap()).unwrap());

    let wasm_tier_cost = extract_wasmi_fuel_costs(&params_wasm);

    if std::env::var("WRITE_PARAMS").is_ok() {
        write_budget_params_code(&params, &wasm_tier_cost);
    }
    Ok(())
}
