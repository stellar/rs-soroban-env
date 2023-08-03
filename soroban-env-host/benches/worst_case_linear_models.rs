// Run this with
// $ cargo bench --features wasmi,testutils --bench worst_case_linear_models -- --nocapture
// You can optionally pass in args listing the {`ContractCostType`, `WasmInsnType`} combination to run with, e.g.
// $ cargo bench --features wasmi,testutils --bench worst_case_linear_models -- VecNew I64Rotr --nocapture
mod common;
use common::*;
use soroban_env_host::{cost_runner::WasmInsnType, xdr::ContractCostType};
use std::{collections::BTreeMap, fmt::Debug, io::Write};
use tabwriter::{Alignment, TabWriter};

struct WorstCaseLinearModels;
impl Benchmark for WorstCaseLinearModels {
    fn bench<HCM: HostCostMeasurement>() -> std::io::Result<(FPCostModel, FPCostModel)> {
        let mut measurements = measure_worst_case_costs::<HCM>(1..20)?;

        measurements.preprocess();
        measurements.report_table();
        let cpu_model = measurements.fit_model_to_cpu();
        let mem_model = measurements.fit_model_to_mem();
        println!("cpu model params: {:?}", cpu_model);
        println!("mem model params: {:?}", mem_model);
        Ok((cpu_model, mem_model))
    }
}

fn write_cost_params_table<T: Debug>(
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
        writeln!(tw, "{:?}\t{}\t{}\t{}\t{}", ty, cpu.0, cpu.1, mem.0, mem.1).unwrap();
    }
    tw.flush()
}

fn write_budget_params_code(
    params: &BTreeMap<ContractCostType, (FPCostModel, FPCostModel)>,
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
        ContractCostType::{:?} => {{ cpu.const_term = {}; cpu.linear_term = {}; }}",
        ContractCostType::WasmInsnExec,
        base_cpu_per_fuel,
        0
    );
    println!(
        "
        // Host cpu insns per wasm \"memory fuel\". This has to be zero since
        // the fuel (representing cpu cost) has been covered by `WasmInsnExec`.
        // The extra cost of mem processing is accounted for by wasmi's
        // `config.memory_bytes_per_fuel` parameter.
        // This type is designated to the mem cost. \n
        ContractCostType::{:?} => {{ cpu.const_term = {}; cpu.linear_term = {}; }}",
        ContractCostType::WasmMemAlloc,
        0,
        0
    );

    for (ty, (cpu, _)) in params
        .iter()
        .map(|(ty, (cpu, mem))| (ty, (cpu.params_as_u64(), mem.params_as_u64())))
    {
        println!(
            "
            ContractCostType::{:?} => {{ cpu.const_term = {}; cpu.linear_term = {}; }}",
            ty, cpu.0, cpu.1
        );
    }
    println!("");
    println!("");

    println!(
        "
        // This type is designated to the cpu cost. By definition, the memory cost\n
        // of a (cpu) fuel is zero.\n
        ContractCostType::{:?} => {{ mem.const_term = {}; mem.linear_term = {}; }}",
        ContractCostType::WasmInsnExec,
        0,
        0
    );
    println!(
        "
        // Bytes per wasmi \"memory fuel\". By definition this has to be a const = 1\n
        // because of the 1-to-1 equivalence of the Wasm mem fuel and a host byte.\n
        ContractCostType::{:?} => {{ mem.const_term = {}; mem.linear_term = {}; }}",
        ContractCostType::WasmMemAlloc,
        1,
        0
    );
    for (ty, (_, mem)) in params
        .iter()
        .map(|(ty, (cpu, mem))| (ty, (cpu.params_as_u64(), mem.params_as_u64())))
    {
        println!(
            "
            ContractCostType::{:?} => {{ mem.const_term = {}; mem.linear_term = {}; }}",
            ty, mem.0, mem.1
        );
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
    params_wasm: &BTreeMap<WasmInsnType, (FPCostModel, FPCostModel)>,
    insn_tier: &[WasmInsnType],
) -> (BTreeMap<WasmInsnType, (FPCostModel, FPCostModel)>, f64) {
    let mut params_tier: BTreeMap<WasmInsnType, (FPCostModel, FPCostModel)> = BTreeMap::new();
    for ty in insn_tier {
        if let Some(res) = params_wasm.get(&ty) {
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
    params_wasm: &BTreeMap<WasmInsnType, (FPCostModel, FPCostModel)>,
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
    params_wasm: &BTreeMap<WasmInsnType, (FPCostModel, FPCostModel)>,
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
    env_logger::init();
    let params = for_each_host_cost_measurement::<WorstCaseLinearModels>()?;
    let params_wasm = for_each_wasm_insn_measurement::<WorstCaseLinearModels>()?;

    let mut tw = TabWriter::new(vec![])
        .padding(5)
        .alignment(Alignment::Right);
    write_cost_params_table::<ContractCostType>(&mut tw, &params)?;
    eprintln!("{}", String::from_utf8(tw.into_inner().unwrap()).unwrap());

    let wasm_tier_cost = extract_wasmi_fuel_costs(&params_wasm);

    if std::env::var("WRITE_PARAMS").is_ok() {
        write_budget_params_code(&params, &wasm_tier_cost);
    }
    Ok(())
}
