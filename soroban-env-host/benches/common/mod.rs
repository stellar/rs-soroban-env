#![allow(dead_code)]

mod cost_types;
mod measure;
mod modelfit;
mod util;

use cost_types::*;
pub use measure::*;
pub use modelfit::*;

use soroban_env_host::{budget::CostType, cost_runner::CostRunner};
use std::collections::BTreeSet;

pub(crate) trait Benchmark {
    fn bench<HCM: HostCostMeasurement>() -> std::io::Result<()>;
}

fn get_explicit_bench_names() -> Option<Vec<String>> {
    let bare_args: Vec<_> = std::env::args().filter(|x| !x.starts_with("-")).collect();
    match bare_args.len() {
        0 | 1 => None,
        _ => Some(bare_args[1..].into()),
    }
}

fn should_run<HCM: HostCostMeasurement>() -> bool {
    if let Some(bench_names) = get_explicit_bench_names() {
        let name = format!("{:?}", <HCM::Runner as CostRunner>::COST_TYPE)
            .split("::")
            .last()
            .unwrap()
            .to_string();
        bench_names.into_iter().find(|arg| *arg == name).is_some()
    } else {
        true
    }
}

fn call_bench<B: Benchmark, HCM: HostCostMeasurement>(
    costs: &mut BTreeSet<CostType>,
) -> std::io::Result<()> {
    if should_run::<HCM>() {
        B::bench::<HCM>()?;
        costs.insert(<HCM::Runner as CostRunner>::COST_TYPE);
    }
    Ok(())
}

pub(crate) fn for_each_host_cost_measurement<B: Benchmark>() -> std::io::Result<()> {
    let mut costs: BTreeSet<CostType> = BTreeSet::new();

    call_bench::<B, ComputeEd25519PubKeyMeasure>(&mut costs)?;
    call_bench::<B, ComputeSha256HashMeasure>(&mut costs)?;
    call_bench::<B, Ed25519ScalarMulMeasure>(&mut costs)?;
    call_bench::<B, ScMapToHostMapMeasure>(&mut costs)?;
    call_bench::<B, ScVecToHostVecMeasure>(&mut costs)?;
    call_bench::<B, VerifyEd25519SigMeasure>(&mut costs)?;
    call_bench::<B, VmInstantiationMeasure>(&mut costs)?;
    call_bench::<B, VmMemReadMeasure>(&mut costs)?;
    call_bench::<B, VmMemWriteMeasure>(&mut costs)?;
    call_bench::<B, WasmInsnExecMeasure>(&mut costs)?;
    call_bench::<B, WasmMemAllocMeasure>(&mut costs)?;
    call_bench::<B, CreateRecordDebugEventMeasure>(&mut costs)?;
    call_bench::<B, RecordContractEventMeasure>(&mut costs)?;
    call_bench::<B, VisitObjectMeasure>(&mut costs)?;
    call_bench::<B, GuardFrameMeasure>(&mut costs)?;
    call_bench::<B, ValXdrConvMeasure>(&mut costs)?;
    call_bench::<B, ValSerMeasure>(&mut costs)?;
    call_bench::<B, ValDeserMeasure>(&mut costs)?;
    call_bench::<B, HostObjAllocSlotMeasure>(&mut costs)?;
    call_bench::<B, BigIntNewMeasure>(&mut costs)?;
    call_bench::<B, BigIntAddSubMeasure>(&mut costs)?;
    call_bench::<B, BigIntMulMeasure>(&mut costs)?;
    call_bench::<B, BigIntDivRemMeasure>(&mut costs)?;
    call_bench::<B, BigIntBitwiseOpMeasure>(&mut costs)?;
    call_bench::<B, BigIntShiftMeasure>(&mut costs)?;
    call_bench::<B, BigIntCmpMeasure>(&mut costs)?;
    call_bench::<B, BigIntGcdLcmMeasure>(&mut costs)?;
    call_bench::<B, BigIntPowMeasure>(&mut costs)?;
    call_bench::<B, BigIntPowModMeasure>(&mut costs)?;
    call_bench::<B, BigIntSqrtMeasure>(&mut costs)?;
    call_bench::<B, BigIntFromBytesMeasure>(&mut costs)?;
    call_bench::<B, BigIntToBytesMeasure>(&mut costs)?;
    call_bench::<B, BigIntToRadixMeasure>(&mut costs)?;
    call_bench::<B, ImMapNewMeasure>(&mut costs)?;
    call_bench::<B, ImMapImmutEntryMeasure>(&mut costs)?;
    call_bench::<B, ImMapMutEntryMeasure>(&mut costs)?;
    call_bench::<B, ImMapCmpMeasure>(&mut costs)?;
    call_bench::<B, ImVecNewMeasure>(&mut costs)?;
    call_bench::<B, ImVecImmutEntryMeasure>(&mut costs)?;
    call_bench::<B, ImVecMutEntryMeasure>(&mut costs)?;
    call_bench::<B, ImVecCmpMeasure>(&mut costs)?;
    call_bench::<B, BytesAppendMeasure>(&mut costs)?;
    call_bench::<B, BytesCmpMeasure>(&mut costs)?;
    call_bench::<B, BytesCloneMeasure>(&mut costs)?;
    call_bench::<B, BytesDelMeasure>(&mut costs)?;
    call_bench::<B, BytesInsertMeasure>(&mut costs)?;
    call_bench::<B, BytesPopMeasure>(&mut costs)?;
    call_bench::<B, BytesPushMeasure>(&mut costs)?;
    call_bench::<B, InvokeVmFunctionMeasure>(&mut costs)?;
    call_bench::<B, InvokeHostFunctionMeasure>(&mut costs)?;
    call_bench::<B, ChargeBudgetMeasure>(&mut costs)?;

    if get_explicit_bench_names().is_none() {
        for cost in CostType::variants() {
            if !costs.contains(cost) {
                eprintln!("warning: missing cost measurement for {:?}", cost);
            }
        }
        // Missing because we need some kind of size limits:
        // CloneEvents
        // ScVecFromHostVec
        // ScMapFromHostMap
        // Storage related: need to know the size limit of keys which are LedgerKeys
    }
    Ok(())
}
