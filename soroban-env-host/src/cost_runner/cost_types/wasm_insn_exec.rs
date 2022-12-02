use crate::{
    budget::CostType,
    cost_runner::CostRunner,
    xdr::{ScVal, ScVec},
    Vm,
};
use std::rc::Rc;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// This is a subset of WASM instructions we are interested in for calibration.
/// Only interested in i64 numeric type (ignores u64, i(u)32, F32(64), unsigned).
pub enum WasmInsnType {
    LocalGet,
    LocalSet,
    LocalTee,
    Br,
    BrIfEqz,
    BrIfNez,
    ReturnIfNez,
    BrTable,
    Unreachable,
    Return,
    Call,
    CallIndirect,
    Drop,
    Select,
    GlobalGet,
    GlobalSet,
    // I32Load,
    I64Load,
    // F32Load,
    // F64Load,
    // I32Load8S,
    // I32Load8U,
    // I32Load16S,
    // I32Load16U,
    I64Load8S,
    // I64Load8U,
    I64Load16S,
    // I64Load16U,
    I64Load32S,
    // I64Load32U,
    // I32Store,
    I64Store,
    // F32Store,
    // F64Store,
    // I32Store8,
    // I32Store16,
    I64Store8,
    I64Store16,
    I64Store32,
    MemorySize,
    MemoryGrow,
    Const,
    // I32Eqz,
    // I32Eq,
    // I32Ne,
    // I32LtS,
    // I32LtU,
    // I32GtS,
    // I32GtU,
    // I32LeS,
    // I32LeU,
    // I32GeS,
    // I32GeU,
    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    // I64LtU,
    I64GtS,
    // I64GtU,
    I64LeS,
    // I64LeU,
    I64GeS,
    // I64GeU,
    // F32Eq,
    // F32Ne,
    // F32Lt,
    // F32Gt,
    // F32Le,
    // F32Ge,
    // F64Eq,
    // F64Ne,
    // F64Lt,
    // F64Gt,
    // F64Le,
    // F64Ge,
    // I32Clz,
    // I32Ctz,
    // I32Popcnt,
    // I32Add,
    // I32Sub,
    // I32Mul,
    // I32DivS,
    // I32DivU,
    // I32RemS,
    // I32RemU,
    // I32And,
    // I32Or,
    // I32Xor,
    // I32Shl,
    // I32ShrS,
    // I32ShrU,
    // I32Rotl,
    // I32Rotr,
    I64Clz,
    I64Ctz,
    I64Popcnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    // I64DivU,
    I64RemS,
    // I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    // I64ShrU,
    I64Rotl,
    I64Rotr,
    // F32Abs,
    // F32Neg,
    // F32Ceil,
    // F32Floor,
    // F32Trunc,
    // F32Nearest,
    // F32Sqrt,
    // F32Add,
    // F32Sub,
    // F32Mul,
    // F32Div,
    // F32Min,
    // F32Max,
    // F32Copysign,
    // F64Abs,
    // F64Neg,
    // F64Ceil,
    // F64Floor,
    // F64Trunc,
    // F64Nearest,
    // F64Sqrt,
    // F64Add,
    // F64Sub,
    // F64Mul,
    // F64Div,
    // F64Min,
    // F64Max,
    // F64Copysign,
    // I32WrapI64,
    // I32TruncSF32,
    // I32TruncUF32,
    // I32TruncSF64,
    // I32TruncUF64,
    // I64ExtendSI32,
    // I64ExtendUI32,
    // I64TruncSF32,
    // I64TruncUF32,
    // I64TruncSF64,
    // I64TruncUF64,
    // F32ConvertSI32,
    // F32ConvertUI32,
    // F32ConvertSI64,
    // F32ConvertUI64,
    // F32DemoteF64,
    // F64ConvertSI32,
    // F64ConvertUI32,
    // F64ConvertSI64,
    // F64ConvertUI64,
    // F64PromoteF32,
    // I32ReinterpretF32,
    // I64ReinterpretF64,
    // F32ReinterpretI32,
    // F64ReinterpretI64,
    // I32Extend8S,
    // I32Extend16S,
    // I64Extend8S,
    // I64Extend16S,
    // I64Extend32S,
    // I32TruncSatF32S,
    // I32TruncSatF32U,
    // I32TruncSatF64S,
    // I32TruncSatF64U,
    // I64TruncSatF32S,
    // I64TruncSatF32U,
    // I64TruncSatF64S,
    // I64TruncSatF64U,
}

impl WasmInsnType {
    pub fn variants() -> std::slice::Iter<'static, WasmInsnType> {
        static VARIANTS: &'static [WasmInsnType] = &[
            WasmInsnType::LocalGet,
            WasmInsnType::LocalSet,
            WasmInsnType::LocalTee,
            WasmInsnType::Br,
            WasmInsnType::BrIfEqz,
            WasmInsnType::BrIfNez,
            WasmInsnType::ReturnIfNez,
            WasmInsnType::BrTable,
            WasmInsnType::Unreachable,
            WasmInsnType::Return,
            WasmInsnType::Call,
            WasmInsnType::CallIndirect,
            WasmInsnType::Drop,
            WasmInsnType::Select,
            WasmInsnType::GlobalGet,
            WasmInsnType::GlobalSet,
            // WasmInsnType::I32Load,
            WasmInsnType::I64Load,
            // WasmInsnType::F32Load,
            // WasmInsnType::F64Load,
            // WasmInsnType::I32Load8S,
            // WasmInsnType::I32Load8U,
            // WasmInsnType::I32Load16S,
            // WasmInsnType::I32Load16U,
            WasmInsnType::I64Load8S,
            // WasmInsnType::I64Load8U,
            WasmInsnType::I64Load16S,
            // WasmInsnType::I64Load16U,
            WasmInsnType::I64Load32S,
            // WasmInsnType::I64Load32U,
            // WasmInsnType::I32Store,
            WasmInsnType::I64Store,
            // WasmInsnType::F32Store,
            // WasmInsnType::F64Store,
            // WasmInsnType::I32Store8,
            // WasmInsnType::I32Store16,
            WasmInsnType::I64Store8,
            WasmInsnType::I64Store16,
            WasmInsnType::I64Store32,
            WasmInsnType::MemorySize,
            WasmInsnType::MemoryGrow,
            WasmInsnType::Const,
            // WasmInsnType::I32Eqz,
            // WasmInsnType::I32Eq,
            // WasmInsnType::I32Ne,
            // WasmInsnType::I32LtS,
            // WasmInsnType::I32LtU,
            // WasmInsnType::I32GtS,
            // WasmInsnType::I32GtU,
            // WasmInsnType::I32LeS,
            // WasmInsnType::I32LeU,
            // WasmInsnType::I32GeS,
            // WasmInsnType::I32GeU,
            WasmInsnType::I64Eqz,
            WasmInsnType::I64Eq,
            WasmInsnType::I64Ne,
            WasmInsnType::I64LtS,
            // WasmInsnType::I64LtU,
            WasmInsnType::I64GtS,
            // WasmInsnType::I64GtU,
            WasmInsnType::I64LeS,
            // WasmInsnType::I64LeU,
            WasmInsnType::I64GeS,
            // WasmInsnType::I64GeU,
            // WasmInsnType::F32Eq,
            // WasmInsnType::F32Ne,
            // WasmInsnType::F32Lt,
            // WasmInsnType::F32Gt,
            // WasmInsnType::F32Le,
            // WasmInsnType::F32Ge,
            // WasmInsnType::F64Eq,
            // WasmInsnType::F64Ne,
            // WasmInsnType::F64Lt,
            // WasmInsnType::F64Gt,
            // WasmInsnType::F64Le,
            // WasmInsnType::F64Ge,
            // WasmInsnType::I32Clz,
            // WasmInsnType::I32Ctz,
            // WasmInsnType::I32Popcnt,
            // WasmInsnType::I32Add,
            // WasmInsnType::I32Sub,
            // WasmInsnType::I32Mul,
            // WasmInsnType::I32DivS,
            // WasmInsnType::I32DivU,
            // WasmInsnType::I32RemS,
            // WasmInsnType::I32RemU,
            // WasmInsnType::I32And,
            // WasmInsnType::I32Or,
            // WasmInsnType::I32Xor,
            // WasmInsnType::I32Shl,
            // WasmInsnType::I32ShrS,
            // WasmInsnType::I32ShrU,
            // WasmInsnType::I32Rotl,
            // WasmInsnType::I32Rotr,
            WasmInsnType::I64Clz,
            WasmInsnType::I64Ctz,
            WasmInsnType::I64Popcnt,
            WasmInsnType::I64Add,
            WasmInsnType::I64Sub,
            WasmInsnType::I64Mul,
            WasmInsnType::I64DivS,
            // WasmInsnType::I64DivU,
            WasmInsnType::I64RemS,
            // WasmInsnType::I64RemU,
            WasmInsnType::I64And,
            WasmInsnType::I64Or,
            WasmInsnType::I64Xor,
            WasmInsnType::I64Shl,
            WasmInsnType::I64ShrS,
            // WasmInsnType::I64ShrU,
            WasmInsnType::I64Rotl,
            WasmInsnType::I64Rotr,
            // WasmInsnType::F32Abs,
            // WasmInsnType::F32Neg,
            // WasmInsnType::F32Ceil,
            // WasmInsnType::F32Floor,
            // WasmInsnType::F32Trunc,
            // WasmInsnType::F32Nearest,
            // WasmInsnType::F32Sqrt,
            // WasmInsnType::F32Add,
            // WasmInsnType::F32Sub,
            // WasmInsnType::F32Mul,
            // WasmInsnType::F32Div,
            // WasmInsnType::F32Min,
            // WasmInsnType::F32Max,
            // WasmInsnType::F32Copysign,
            // WasmInsnType::F64Abs,
            // WasmInsnType::F64Neg,
            // WasmInsnType::F64Ceil,
            // WasmInsnType::F64Floor,
            // WasmInsnType::F64Trunc,
            // WasmInsnType::F64Nearest,
            // WasmInsnType::F64Sqrt,
            // WasmInsnType::F64Add,
            // WasmInsnType::F64Sub,
            // WasmInsnType::F64Mul,
            // WasmInsnType::F64Div,
            // WasmInsnType::F64Min,
            // WasmInsnType::F64Max,
            // WasmInsnType::F64Copysign,
            // WasmInsnType::I32WrapI64,
            // WasmInsnType::I32TruncSF32,
            // WasmInsnType::I32TruncUF32,
            // WasmInsnType::I32TruncSF64,
            // WasmInsnType::I32TruncUF64,
            // WasmInsnType::I64ExtendSI32,
            // WasmInsnType::I64ExtendUI32,
            // WasmInsnType::I64TruncSF32,
            // WasmInsnType::I64TruncUF32,
            // WasmInsnType::I64TruncSF64,
            // WasmInsnType::I64TruncUF64,
            // WasmInsnType::F32ConvertSI32,
            // WasmInsnType::F32ConvertUI32,
            // WasmInsnType::F32ConvertSI64,
            // WasmInsnType::F32ConvertUI64,
            // WasmInsnType::F32DemoteF64,
            // WasmInsnType::F64ConvertSI32,
            // WasmInsnType::F64ConvertUI32,
            // WasmInsnType::F64ConvertSI64,
            // WasmInsnType::F64ConvertUI64,
            // WasmInsnType::F64PromoteF32,
            // WasmInsnType::I32ReinterpretF32,
            // WasmInsnType::I64ReinterpretF64,
            // WasmInsnType::F32ReinterpretI32,
            // WasmInsnType::F64ReinterpretI64,
            // WasmInsnType::I32Extend8S,
            // WasmInsnType::I32Extend16S,
            // WasmInsnType::I64Extend8S,
            // WasmInsnType::I64Extend16S,
            // WasmInsnType::I64Extend32S,
            // WasmInsnType::I32TruncSatF32S,
            // WasmInsnType::I32TruncSatF32U,
            // WasmInsnType::I32TruncSatF64S,
            // WasmInsnType::I32TruncSatF64U,
            // WasmInsnType::I64TruncSatF32S,
            // WasmInsnType::I64TruncSatF32U,
            // WasmInsnType::I64TruncSatF64S,
            // WasmInsnType::I64TruncSatF64U,
        ];
        VARIANTS.iter()
    }
}

#[derive(Clone)]
pub struct WasmInsnSample {
    pub insns: u64,
    pub vm: Rc<Vm>,
}

#[derive(Clone)]
pub struct WasmInsnExecSample {
    pub insns: u64,
    pub args: ScVec,
    pub vm: Rc<Vm>,
}

pub struct WasmInsnExecRun;
impl CostRunner for WasmInsnExecRun {
    const COST_TYPE: CostType = CostType::WasmInsnExec;
    type SampleType = WasmInsnExecSample;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        let scval = sample
            .vm
            .invoke_function(host, "test", &sample.args)
            .unwrap();
        assert_eq!(scval, ScVal::Symbol("pass".try_into().unwrap()));
    }

    fn get_total_input(_host: &crate::Host, sample: &Self::SampleType) -> u64 {
        sample.insns * 4 // TODO: avoid magic number
    }
}

pub struct WasmMemAllocRun;
impl CostRunner for WasmMemAllocRun {
    const COST_TYPE: CostType = CostType::WasmMemAlloc;

    const RUN_ITERATIONS: u64 = 1;

    type SampleType = (Rc<Vm>, usize);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.0.invoke_function_raw(host, "test", &[]).unwrap();
    }

    fn get_total_input(_host: &crate::Host, sample: &Self::SampleType) -> u64 {
        (sample.1 as u64) * Self::RUN_ITERATIONS * 65536
    }
}

macro_rules! impl_wasm_insn_runner {
    ($runner:ident, $insn:ident) => {
        pub struct $runner;

        impl $runner {
            pub const INSN_TYPE: WasmInsnType = WasmInsnType::$insn;
        }

        impl CostRunner for $runner {
            type SampleType = WasmInsnSample;
            const COST_TYPE: CostType = CostType::WasmInsnExec;

            fn run_iter(host: &crate::Host, _iter: u64, sample: WasmInsnSample) {
                sample.vm.invoke_function_raw(host, "test", &[]).unwrap();
            }

            fn get_total_input(_host: &crate::Host, sample: &WasmInsnSample) -> u64 {
                (sample.insns as u64) * Self::RUN_ITERATIONS
            }
        }
    };
}

impl_wasm_insn_runner!(SelectRun, Select);
impl_wasm_insn_runner!(BrRun, Br);
impl_wasm_insn_runner!(ConstRun, Const);
impl_wasm_insn_runner!(LocalGetRun, LocalGet);
impl_wasm_insn_runner!(LocalSetRun, LocalSet);
impl_wasm_insn_runner!(LocalTeeRun, LocalTee);
impl_wasm_insn_runner!(CallRun, Call);
impl_wasm_insn_runner!(CallIndirectRun, CallIndirect);
impl_wasm_insn_runner!(GlobalGetRun, GlobalGet);
impl_wasm_insn_runner!(GlobalSetRun, GlobalSet);
impl_wasm_insn_runner!(I64StoreRun, I64Store);
impl_wasm_insn_runner!(I64LoadRun, I64Load);
impl_wasm_insn_runner!(I64Load8SRun, I64Load8S);
impl_wasm_insn_runner!(I64Load16SRun, I64Load16S);
impl_wasm_insn_runner!(I64Load32SRun, I64Load32S);
impl_wasm_insn_runner!(I64Store8Run, I64Store8);
impl_wasm_insn_runner!(I64Store16Run, I64Store16);
impl_wasm_insn_runner!(I64Store32Run, I64Store32);
impl_wasm_insn_runner!(MemorySizeRun, MemorySize);
impl_wasm_insn_runner!(MemoryGrowRun, MemoryGrow);
impl_wasm_insn_runner!(I64ClzRun, I64Clz);
impl_wasm_insn_runner!(I64CtzRun, I64Ctz);
impl_wasm_insn_runner!(I64PopcntRun, I64Popcnt);
impl_wasm_insn_runner!(I64EqzRun, I64Eqz);
impl_wasm_insn_runner!(I64EqRun, I64Eq);
impl_wasm_insn_runner!(I64NeRun, I64Ne);
impl_wasm_insn_runner!(I64LtSRun, I64LtS);
impl_wasm_insn_runner!(I64GtSRun, I64GtS);
impl_wasm_insn_runner!(I64LeSRun, I64LeS);
impl_wasm_insn_runner!(I64GeSRun, I64GeS);
impl_wasm_insn_runner!(I64AddRun, I64Add);
impl_wasm_insn_runner!(I64SubRun, I64Sub);
impl_wasm_insn_runner!(I64MulRun, I64Mul);
impl_wasm_insn_runner!(I64DivSRun, I64DivS);
impl_wasm_insn_runner!(I64RemSRun, I64RemS);
impl_wasm_insn_runner!(I64AndRun, I64And);
impl_wasm_insn_runner!(I64OrRun, I64Or);
impl_wasm_insn_runner!(I64XorRun, I64Xor);
impl_wasm_insn_runner!(I64ShlRun, I64Shl);
impl_wasm_insn_runner!(I64ShrSRun, I64ShrS);
impl_wasm_insn_runner!(I64RotlRun, I64Rotl);
impl_wasm_insn_runner!(I64RotrRun, I64Rotr);
