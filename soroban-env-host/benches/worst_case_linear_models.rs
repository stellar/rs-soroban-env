// Run this with
// $ cargo bench --features bench --bench worst_case_linear_models -- --nocapture
// You can optionally pass in args listing the {`ContractCostType`, `WasmInsnType`} combination to run with, e.g.
// $ cargo bench --features bench --bench worst_case_linear_models -- MemCpy I64Rotr --nocapture
// To run the experimental cost types: $ RUN_EXPERIMENT=1 cargo bench ...
mod common;
use common::*;
use soroban_env_host::{
    budget::MeteredCostComponent,
    cost_runner::{CostRunner, CostType, WasmInsnType},
    xdr::ContractCostType,
};
use std::{collections::BTreeMap, fmt::Display, io::Write};
use tabwriter::{Alignment, TabWriter};

struct WorstCaseLinearModels;
impl Benchmark for WorstCaseLinearModels {
    fn bench<HCM: HostCostMeasurement>(
    ) -> std::io::Result<(MeteredCostComponent, MeteredCostComponent)> {
        let floor = std::env::var("FLOOR")
            .ok()
            .map(|v| v.parse::<u64>().ok())
            .flatten()
            .unwrap_or(0);
        let range = std::env::var("RANGE")
            .ok()
            .map(|v| v.parse::<u64>().ok())
            .flatten()
            .unwrap_or(20);
        let mut measurements = measure_worst_case_costs::<HCM>(floor..range)?;
        measurements.check_range_against_baseline(&HCM::Runner::COST_TYPE)?;
        measurements.preprocess();
        measurements.report_table();
        let (cpu_model, cpu_r2) = measurements.fit_model_to_cpu();
        let (mem_model, mem_r2) = measurements.fit_model_to_mem();
        println!(
            "{:?} cpu: {:?}, R2 score: {}",
            HCM::Runner::COST_TYPE,
            cpu_model,
            cpu_r2
        );
        println!(
            "{:?} mem: {:?}, R2 score: {}",
            HCM::Runner::COST_TYPE,
            mem_model,
            mem_r2
        );
        Ok((cpu_model, mem_model))
    }
}

fn write_cost_params_table<T: Display>(
    tw: &mut TabWriter<Vec<u8>>,
    params: &BTreeMap<T, (MeteredCostComponent, MeteredCostComponent)>,
) -> std::io::Result<()> {
    writeln!(tw, "").unwrap();
    writeln!(tw, "").unwrap();
    writeln!(tw, "cost_type\tcpu_model_const_param\tcpu_model_lin_param\tmem_model_const_param\tmem_model_lin_param").unwrap();

    for (ty, (cpu, mem)) in params.iter() {
        writeln!(
            tw,
            "{}\t{}\t{}\t{}\t{}",
            ty, cpu.const_term, cpu.lin_term, mem.const_term, mem.lin_term
        )
        .unwrap();
    }
    tw.flush()
}

fn correct_multi_variable_models(
    params: &mut BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)>,
) {
    // Several cost types actually represent additional terms a cost model that
    // we're decomposing into multiple variables, such as the cost of VM
    // instantiation. When we charge these costs, we charge each variable
    // separately, i.e. to charge a 5-variable cost we'll make 5 calls to the
    // budget. Only the first of these 5 calls should have a constant factor,
    // the rest should have zero as their constant (since they only contribute a
    // new linear term), but the calibration code will have put the same (or
    // nearly-the-same) nonzero constant term in each `CostComponent`. We
    // correct this here by zeroing out the constant term in all but the first
    // `CostComponent` of each set, (and attempting to confirm that they all
    // have roughly-the-same constant term).
    use ContractCostType::*;
    const MULTI_VARIABLE_COST_GROUPS: &[&[ContractCostType]] = &[
        &[
            ParseWasmInstructions,
            ParseWasmFunctions,
            ParseWasmGlobals,
            ParseWasmTableEntries,
            ParseWasmTypes,
            ParseWasmDataSegments,
            ParseWasmElemSegments,
            ParseWasmImports,
            ParseWasmExports,
            ParseWasmDataSegmentBytes,
        ],
        &[
            InstantiateWasmInstructions,
            InstantiateWasmFunctions,
            InstantiateWasmGlobals,
            InstantiateWasmTableEntries,
            InstantiateWasmTypes,
            InstantiateWasmDataSegments,
            InstantiateWasmElemSegments,
            InstantiateWasmImports,
            InstantiateWasmExports,
            InstantiateWasmDataSegmentBytes,
        ],
    ];
    for group in MULTI_VARIABLE_COST_GROUPS {
        let mut iter = group.iter();
        if let Some(first) = iter.next() {
            let Some((first_cpu, first_mem)) = params.get(&CostType::Contract(*first)).cloned()
            else {
                continue;
            };
            for ty in iter {
                let Some((cpu, mem)) = params.get_mut(&CostType::Contract(*ty)) else {
                    continue;
                };
                let cpu_const_diff_ratio = (cpu.const_term as f64 - first_cpu.const_term as f64)
                    / first_cpu.const_term as f64;
                let mem_const_diff_ratio = (mem.const_term as f64 - first_mem.const_term as f64)
                    / first_mem.const_term as f64;
                assert!(
                    cpu_const_diff_ratio < 0.25,
                    "cost type {:?} has too large a constant CPU term over {:?}: {:?} vs. {:?} ({:?} diff)",
                    ty,
                    first,
                    cpu.const_term,
                    first_cpu.const_term,
                    cpu_const_diff_ratio
                );
                assert!(
                    mem_const_diff_ratio < 0.25,
                    "cost type {:?} has too large a constant memory term over {:?}: {:?} vs. {:?} ({:?} diff)",
                    ty,
                    first,
                    mem.const_term,
                    first_mem.const_term,
                    mem_const_diff_ratio
                );
                cpu.const_term = 0;
                mem.const_term = 0;
            }
        }
    }
}

fn write_budget_params_code(
    params: &BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)>,
    wasm_tier_cost: &BTreeMap<WasmInsnTier, u64>,
) {
    println!("");
    println!("");

    let base_cpu_per_fuel = wasm_tier_cost[&WasmInsnTier::BASE];
    let entity_cpu_per_fuel = wasm_tier_cost[&WasmInsnTier::ENTITY];
    let load_cpu_per_fuel = wasm_tier_cost[&WasmInsnTier::LOAD];
    let store_cpu_per_fuel = wasm_tier_cost[&WasmInsnTier::STORE];
    let call_cpu_per_fuel = wasm_tier_cost[&WasmInsnTier::CALL];

    // first print the cpu part

    for ty in ContractCostType::VARIANTS.iter() {
        match ty {
            ContractCostType::WasmInsnExec => {
                println!(
                    "
                    // This is the host cpu insn cost per wasm \"fuel\". Every \"base\" wasm
                    // instruction costs 1 fuel (by default), and some particular types of
                    // instructions may cost additional amount of fuel based on
                    // wasmi's config setting. \n
                    ContractCostType::{:?} => {{ cpu.const_term = {}; cpu.lin_term = ScaledU64({}); }}",
                    ty,
                    base_cpu_per_fuel,
                    0
                );
            }
            ContractCostType::MemAlloc => {
                println!(
                    "
                    // We don't have a clear way of modeling the linear term of
                    // memalloc cost thus we choose a reasonable upperbound which is
                    // same as other mem ops.\n
                    ContractCostType::{:?} => {{ cpu.const_term = 434; cpu.lin_term = ScaledU64::from_unscaled_u64(1).safe_div(8); }}",
                    ty,
                );
            }
            ContractCostType::MemCpy => {
                println!(
                    "
                    // We don't use a calibrated number for this because sending a
                    // large calibration-buffer to memcpy hits an optimized
                    // large-memcpy path in the stdlib, which has both a large
                    // overhead and a small per-byte cost. But large buffers aren't
                    // really how byte-copies usually get used in metered code. Most
                    // calls have to do with small copies of a few tens or hundreds
                    // of bytes. So instead we just \"reason it out\": we can probably
                    // copy 8 bytes per instruction on a 64-bit machine, and that
                    // therefore a 1-byte copy is considered 1/8th of an
                    // instruction. We also add in a nonzero constant overhead, to
                    // avoid having anything that can be zero cost and approximate
                    // whatever function call, arg-shuffling, spills, reloads or
                    // other flotsam accumulates around a typical memory copy.\n
                    ContractCostType::{:?} => {{ cpu.const_term = 42; cpu.lin_term = ScaledU64::from_unscaled_u64(1).safe_div(8); }}",
                    ty,
                );
            }
            ContractCostType::MemCmp => {
                println!(
                    "
                    // This is analytical.
                    ContractCostType::{:?} => {{ cpu.const_term = 44; cpu.lin_term = ScaledU64::from_unscaled_u64(1).safe_div(8); }}",
                    ty,
                );
            }
            ContractCostType::VmCachedInstantiation => {
                println!(
                    " 
                    // `VmCachedInstantiation` has not been calibrated, it is copied
                    // from `VmInstantiation`.\n
                    "
                );
                match params.get(&CostType::Contract(ContractCostType::VmInstantiation)) {
                    Some((cpu, _)) => println!(
                        "ContractCostType::VmCachedInstantiation => {{ cpu.const_term = {}; cpu.lin_term = {:?}; }}",
                        cpu.const_term, cpu.lin_term
                    ),
                    None => println!(
                        "ContractCostType::VmCachedInstantiation => todo!()"
                    ),
                }
            }
            _ => match params.get(&CostType::Contract(*ty)) {
                Some((cpu, _)) => println!(
                    "ContractCostType::{:?} => {{ cpu.const_term = {}; cpu.lin_term = {:?}; }}",
                    ty, cpu.const_term, cpu.lin_term
                ),
                None => println!("ContractCostType::{:?} => todo!()", ty),
            },
        }
    }

    println!("");
    println!("");

    // next print the mem part

    for ty in ContractCostType::VARIANTS.iter() {
        match ty {
            ContractCostType::WasmInsnExec => {
                println!(
                    "
                    // This type is designated to the cpu cost. By definition, the
                    // memory cost of a (cpu) fuel is zero.\n
                    ContractCostType::{:?} => {{ mem.const_term = {}; mem.lin_term = ScaledU64({}); }}",
                    ty, 0, 0
                )
            }
            ContractCostType::MemAlloc => {
                println!(
                    "// This is analytical.\n
                    ContractCostType::{:?} => {{ mem.const_term = 16; mem.lin_term = ScaledU64::from_unscaled_u64(1); }}",
                    ty
                )
            }
            ContractCostType::MemCmp | ContractCostType::MemCpy => {
                println!(
                    "// This is analytical.\n
                    ContractCostType::{:?} => {{ mem.const_term = 0; mem.lin_term = ScaledU64(0); }}",
                    ty
                )
            }
            ContractCostType::ValSer => {
                println!(
                    "
                    // This is analytically derived from calibration on highly nested
                    // xdr structures.\n
                    ContractCostType::{:?} => {{ mem.const_term = 242; mem.lin_term = ScaledU64::from_unscaled_u64(3); }}",
                    ty
                )
            }
            ContractCostType::ValDeser => {
                println!(
                    "
                    // This is analytically derived from calibration on highly nested
                    // xdr structures.\n
                    ContractCostType::{:?} => {{ mem.const_term = 0; mem.lin_term = ScaledU64::from_unscaled_u64(3); }}",
                    ty
                )
            }
            ContractCostType::VmCachedInstantiation => {
                println!(
                    " 
                    // `VmCachedInstantiation` has not been calibrated, it is copied
                    // from `VmInstantiation`.\n
                    "
                );
                match params.get(&CostType::Contract(ContractCostType::VmInstantiation)) {
                    Some((_, mem)) => println!(
                        "ContractCostType::VmCachedInstantiation => {{ mem.const_term = {}; mem.lin_term = {:?}; }}",
                        mem.const_term, mem.lin_term
                    ),
                    None => println!(
                        "ContractCostType::VmCachedInstantiation => todo!()"
                    ),
                }
            }
            _ => match params.get(&CostType::Contract(*ty)) {
                Some((_, mem)) => println!(
                    "ContractCostType::{:?} => {{ mem.const_term = {}; mem.lin_term = {:?}; }}",
                    ty, mem.const_term, mem.lin_term
                ),
                None => println!("ContractCostType::{:?} => todo!()", ty),
            },
        }
    }

    println!("");
    println!("");
    println!(
        "
        FuelConfig {{base: {}, entity: {}, load: {}, store: {}, call: {}}}",
        1,
        (entity_cpu_per_fuel
            .checked_div(base_cpu_per_fuel)
            .unwrap_or(0))
        .max(1),
        (load_cpu_per_fuel
            .checked_div(base_cpu_per_fuel)
            .unwrap_or(0))
        .max(1),
        (store_cpu_per_fuel
            .checked_div(base_cpu_per_fuel)
            .unwrap_or(0))
        .max(1),
        (call_cpu_per_fuel
            .checked_div(base_cpu_per_fuel)
            .unwrap_or(0))
        .max(1)
    )
}

fn extract_tier(
    params_wasm: &BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)>,
    insn_tier: &[WasmInsnType],
) -> (
    BTreeMap<WasmInsnType, (MeteredCostComponent, MeteredCostComponent)>,
    u64,
) {
    let mut params_tier: BTreeMap<WasmInsnType, (MeteredCostComponent, MeteredCostComponent)> =
        BTreeMap::new();
    for ty in insn_tier {
        if let Some(res) = params_wasm.get(&CostType::Wasm(*ty)) {
            params_tier.insert(ty.clone(), res.clone());
        }
    }

    let cpu_per_fuel: Vec<u64> = params_tier
        .iter()
        .map(|(_, (cpu, _))| cpu.const_term)
        .collect();
    let ave_cpu_per_fuel = cpu_per_fuel
        .iter()
        .sum::<u64>()
        .checked_div(cpu_per_fuel.len() as u64)
        .unwrap_or(0);
    (params_tier, ave_cpu_per_fuel)
}

fn process_tier(
    tier: WasmInsnTier,
    params_wasm: &BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)>,
    insn_tier: &[WasmInsnType],
) -> u64 {
    let (params_tier, ave_cpu_per_fuel) = extract_tier(params_wasm, insn_tier);

    if !params_tier.is_empty() {
        println!("\n");
        println!("\n{:=<100}", "");
        println!("\"{:?}\" tier", tier);

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
    }
    ave_cpu_per_fuel
}

fn extract_wasmi_fuel_costs(
    params_wasm: &BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)>,
) -> BTreeMap<WasmInsnTier, u64> {
    let base_cost = process_tier(WasmInsnTier::BASE, params_wasm, &WASM_INSN_BASE);
    let entity_cost = process_tier(WasmInsnTier::ENTITY, params_wasm, &WASM_INSN_ENTITY);
    let load_cost = process_tier(WasmInsnTier::LOAD, params_wasm, &WASM_INSN_LOAD);
    let store_cost = process_tier(WasmInsnTier::STORE, params_wasm, &WASM_INSN_STORE);
    let call_cost = process_tier(WasmInsnTier::CALL, params_wasm, &WASM_INSN_CALL);
    let mut res: BTreeMap<WasmInsnTier, u64> = BTreeMap::new();
    res.insert(WasmInsnTier::BASE, base_cost);
    res.insert(WasmInsnTier::ENTITY, entity_cost);
    res.insert(WasmInsnTier::LOAD, load_cost);
    res.insert(WasmInsnTier::STORE, store_cost);
    res.insert(WasmInsnTier::CALL, call_cost);
    res
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn main() -> std::io::Result<()> {
    let mut params = if std::env::var("RUN_EXPERIMENT").is_err() {
        for_each_host_cost_measurement::<WorstCaseLinearModels>()?
    } else {
        for_each_experimental_cost_measurement::<WorstCaseLinearModels>()?
    };
    let params_wasm = if std::env::var("SKIP_WASM_INSNS").is_err() {
        for_each_wasm_insn_measurement::<WorstCaseLinearModels>()?
    } else {
        BTreeMap::new()
    };

    correct_multi_variable_models(&mut params);

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
