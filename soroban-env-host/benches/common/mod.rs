#![allow(dead_code)]

mod cost_types;
mod experimental;
mod measure;
mod modelfit;
mod util;

use cost_types::*;
use experimental::*;
pub use measure::*;

use soroban_env_common::xdr::Name;
use soroban_env_host::{
    budget::MeteredCostComponent,
    cost_runner::{CostRunner, CostType, WasmInsnType},
    xdr::ContractCostType,
};
use std::collections::BTreeMap;

pub(crate) trait Benchmark {
    fn bench<HCM: HostCostMeasurement>(
    ) -> std::io::Result<(MeteredCostComponent, MeteredCostComponent)>;
}

fn get_explicit_bench_names() -> Option<Vec<String>> {
    let bare_args: Vec<_> = std::env::args().filter(|x| !x.starts_with('-')).collect();
    match bare_args.len() {
        0 | 1 => None,
        _ => Some(bare_args[1..].into()),
    }
}

fn should_run<HCM: HostCostMeasurement>() -> bool {
    if let Some(bench_names) = get_explicit_bench_names() {
        let name = <HCM::Runner as CostRunner>::COST_TYPE.name();
        bench_names.iter().any(|arg| *arg == name)
    } else {
        true
    }
}

fn call_bench<B: Benchmark, HCM: HostCostMeasurement>(
    params: &mut BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)>,
) -> std::io::Result<()> {
    if should_run::<HCM>() {
        params.insert(<HCM::Runner as CostRunner>::COST_TYPE, B::bench::<HCM>()?);
    }
    Ok(())
}

pub(crate) fn for_each_experimental_cost_measurement<B: Benchmark>(
) -> std::io::Result<BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)>> {
    let mut params: BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)> =
        BTreeMap::new();
    call_bench::<B, Ed25519ScalarMulMeasure>(&mut params)?;
    call_bench::<B, VerifyEd25519SigMeasure>(&mut params)?;
    call_bench::<B, ReadXdrByteArrayMeasure>(&mut params)?;
    call_bench::<B, EcdsaSecp256k1VerifyMeasure>(&mut params)?;
    call_bench::<B, EcdsaSecp256r1RecoverMeasure>(&mut params)?;
    call_bench::<B, Sec1DecodePointCompressedMeasure>(&mut params)?;
    call_bench::<B, DecodeSecp256r1SigMeasure>(&mut params)?;
    Ok(params)
}

pub(crate) fn for_each_host_cost_measurement<B: Benchmark>(
) -> std::io::Result<BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)>> {
    let mut params: BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)> =
        BTreeMap::new();

    call_bench::<B, ComputeEd25519PubKeyMeasure>(&mut params)?;
    call_bench::<B, ComputeKeccak256HashMeasure>(&mut params)?;
    call_bench::<B, ComputeSha256HashMeasure>(&mut params)?;
    call_bench::<B, RecoverEcdsaSecp256k1KeyMeasure>(&mut params)?;
    call_bench::<B, VerifyEd25519SigMeasure>(&mut params)?;
    call_bench::<B, VmInstantiationMeasure>(&mut params)?;
    call_bench::<B, VisitObjectMeasure>(&mut params)?;
    call_bench::<B, ValDeserMeasure>(&mut params)?;
    call_bench::<B, ValSerMeasure>(&mut params)?;
    call_bench::<B, InvokeVmFunctionMeasure>(&mut params)?;
    call_bench::<B, InvokeHostFunctionMeasure>(&mut params)?;
    call_bench::<B, Int256AddSubMeasure>(&mut params)?;
    call_bench::<B, Int256MulMeasure>(&mut params)?;
    call_bench::<B, Int256DivMeasure>(&mut params)?;
    call_bench::<B, Int256PowMeasure>(&mut params)?;
    call_bench::<B, Int256ShiftMeasure>(&mut params)?;
    call_bench::<B, ChaCha20DrawBytesMeasure>(&mut params)?;

    // P21 cost types
    call_bench::<B, VmCachedInstantiationMeasure>(&mut params)?;

    call_bench::<B, ParseWasmInstructionsMeasure>(&mut params)?;
    call_bench::<B, ParseWasmFunctionsMeasure>(&mut params)?;
    call_bench::<B, ParseWasmGlobalsMeasure>(&mut params)?;
    call_bench::<B, ParseWasmTableEntriesMeasure>(&mut params)?;
    call_bench::<B, ParseWasmTypesMeasure>(&mut params)?;
    call_bench::<B, ParseWasmDataSegmentsMeasure>(&mut params)?;
    call_bench::<B, ParseWasmElemSegmentsMeasure>(&mut params)?;
    call_bench::<B, ParseWasmImportsMeasure>(&mut params)?;
    call_bench::<B, ParseWasmExportsMeasure>(&mut params)?;
    call_bench::<B, ParseWasmDataSegmentBytesMeasure>(&mut params)?;

    call_bench::<B, InstantiateWasmInstructionsMeasure>(&mut params)?;
    call_bench::<B, InstantiateWasmFunctionsMeasure>(&mut params)?;
    call_bench::<B, InstantiateWasmGlobalsMeasure>(&mut params)?;
    call_bench::<B, InstantiateWasmTableEntriesMeasure>(&mut params)?;
    call_bench::<B, InstantiateWasmTypesMeasure>(&mut params)?;
    call_bench::<B, InstantiateWasmDataSegmentsMeasure>(&mut params)?;
    call_bench::<B, InstantiateWasmElemSegmentsMeasure>(&mut params)?;
    call_bench::<B, InstantiateWasmImportsMeasure>(&mut params)?;
    call_bench::<B, InstantiateWasmExportsMeasure>(&mut params)?;
    call_bench::<B, InstantiateWasmDataSegmentBytesMeasure>(&mut params)?;

    call_bench::<B, DecodeEcdsaCurve256SigMeasure>(&mut params)?;
    call_bench::<B, Sec1DecodePointUncompressedMeasure>(&mut params)?;
    call_bench::<B, VerifyEcdsaSecp256r1SigMeasure>(&mut params)?;

    // These three mem ones are derived analytically, we do not calibrate them typically
    if std::env::var("INCLUDE_ANALYTICAL_COSTTYPES").is_ok() {
        call_bench::<B, MemAllocMeasure>(&mut params)?;
        call_bench::<B, MemCpyMeasure>(&mut params)?;
        call_bench::<B, MemCmpMeasure>(&mut params)?;
    }

    if get_explicit_bench_names().is_none() {
        for cost in ContractCostType::variants() {
            if !params.contains_key(&CostType::Contract(cost)) {
                eprintln!("warning: missing cost measurement for {:?}", cost);
            }
        }
    }
    Ok(params)
}

macro_rules! run_wasm_insn_measurement {
    ( $($HCM: ident),* ) => {
        pub(crate) fn for_each_wasm_insn_measurement<B: Benchmark>() -> std::io::Result<BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)>> {
            let mut params: BTreeMap<CostType, (MeteredCostComponent, MeteredCostComponent)> = BTreeMap::new();
            $(
                if should_run::<$HCM>() {
                    params.insert(<$HCM as HostCostMeasurement>::Runner::COST_TYPE, B::bench::<$HCM>()?);
                }
            )*

            if get_explicit_bench_names().is_none() {
                for insn in WasmInsnType::variants() {
                    if !params.contains_key(&CostType::Wasm(*insn)) {
                        eprintln!("warning: missing cost measurement for {:?}", insn);
                    }
                }
            }
            Ok(params)
        }
    };
}
run_wasm_insn_measurement!(
    WasmSelectMeasure,
    WasmBrMeasure,
    WasmBrTableMeasure,
    WasmConstMeasure,
    WasmDropMeasure,
    WasmLocalGetMeasure,
    WasmLocalSetMeasure,
    WasmLocalTeeMeasure,
    WasmCallLocalMeasure,
    WasmCallImportMeasure,
    WasmCallIndirectMeasure,
    WasmGlobalGetMeasure,
    WasmGlobalSetMeasure,
    WasmI64StoreMeasure,
    WasmI64Store8Measure,
    WasmI64Store16Measure,
    WasmI64Store32Measure,
    WasmI64LoadMeasure,
    WasmI64Load8Measure,
    WasmI64Load16Measure,
    WasmI64Load32Measure,
    WasmMemorySizeMeasure,
    WasmMemoryGrowMeasure,
    WasmI64ClzMeasure,
    WasmI64CtzMeasure,
    WasmI64PopcntMeasure,
    WasmI64EqzMeasure,
    WasmI64EqMeasure,
    WasmI64NeMeasure,
    WasmI64LtSMeasure,
    WasmI64GtSMeasure,
    WasmI64LeSMeasure,
    WasmI64GeSMeasure,
    WasmI64AddMeasure,
    WasmI64SubMeasure,
    WasmI64MulMeasure,
    WasmI64DivSMeasure,
    WasmI64RemSMeasure,
    WasmI64AndMeasure,
    WasmI64OrMeasure,
    WasmI64XorMeasure,
    WasmI64ShlMeasure,
    WasmI64ShrSMeasure,
    WasmI64RotlMeasure,
    WasmI64RotrMeasure
);

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
pub(crate) enum WasmInsnTier {
    BASE = 0,
    ENTITY = 1,
    LOAD = 2,
    STORE = 3,
    CALL = 4,
}

pub(crate) const WASM_INSN_BASE: [WasmInsnType; 30] = [
    WasmInsnType::LocalGet,
    WasmInsnType::LocalSet,
    WasmInsnType::LocalTee,
    WasmInsnType::Br,
    WasmInsnType::BrTable,
    WasmInsnType::Drop,
    WasmInsnType::Select,
    WasmInsnType::Const,
    WasmInsnType::I64Eqz,
    WasmInsnType::I64Eq,
    WasmInsnType::I64Ne,
    WasmInsnType::I64LtS,
    WasmInsnType::I64GtS,
    WasmInsnType::I64LeS,
    WasmInsnType::I64GeS,
    WasmInsnType::I64Clz,
    WasmInsnType::I64Ctz,
    WasmInsnType::I64Popcnt,
    WasmInsnType::I64Add,
    WasmInsnType::I64Sub,
    WasmInsnType::I64Mul,
    WasmInsnType::I64DivS,
    WasmInsnType::I64RemS,
    WasmInsnType::I64And,
    WasmInsnType::I64Or,
    WasmInsnType::I64Xor,
    WasmInsnType::I64Shl,
    WasmInsnType::I64ShrS,
    WasmInsnType::I64Rotl,
    WasmInsnType::I64Rotr,
];

pub(crate) const WASM_INSN_ENTITY: [WasmInsnType; 3] = [
    WasmInsnType::GlobalGet,
    WasmInsnType::GlobalSet,
    WasmInsnType::MemorySize,
];

pub(crate) const WASM_INSN_LOAD: [WasmInsnType; 4] = [
    WasmInsnType::I64Load,
    WasmInsnType::I64Load8S,
    WasmInsnType::I64Load16S,
    WasmInsnType::I64Load32S,
];

pub(crate) const WASM_INSN_STORE: [WasmInsnType; 4] = [
    WasmInsnType::I64Store,
    WasmInsnType::I64Store8,
    WasmInsnType::I64Store16,
    WasmInsnType::I64Store32,
];

pub(crate) const WASM_INSN_CALL: [WasmInsnType; 3] = [
    WasmInsnType::CallLocal,
    WasmInsnType::CallImport,
    WasmInsnType::CallIndirect,
];
