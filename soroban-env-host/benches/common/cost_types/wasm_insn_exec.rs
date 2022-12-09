use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{
    cost_runner::*,
    xdr::{Hash, ScVal, ScVec},
    Host, Symbol, Vm,
};
use soroban_synth_wasm::{Arity, GlobalRef, ModEmitter, Operand};

const INSNS_OVERHEAD_CONST: u64 = 21; // measured by `push_const`
const INSNS_OVERHEAD_DROP: u64 = 17; // measured by `drop`

struct WasmModule {
    wasm: Vec<u8>,
    overhead: u64,
}

fn wasm_module_with_4n_insns(n: usize) -> Vec<u8> {
    let mut fe = ModEmitter::new().func(Arity(1), 0);
    let arg = fe.args[0];
    fe.push(Operand::Const64(1));
    for i in 0..n {
        fe.push(arg);
        fe.push(Operand::Const64(i as i64));
        fe.i64_mul();
        fe.i64_add();
    }
    fe.drop();
    fe.push(Symbol::from_str("pass"));
    fe.finish_and_export("test").finish()
}

fn wasm_module_with_mem_grow(n_pages: usize) -> Vec<u8> {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    fe.push(Operand::Const32(n_pages as i32));
    fe.memory_grow();
    // need to drop the return value on the stack because it's an i32
    // and the function needs an i64 return value.
    fe.drop();
    fe.push(Symbol::from_str("pass"));
    fe.finish_and_export("test").finish()
}

// 21 insns / input
fn push_const(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    for i in 0..n {
        fe.i32_const(i as i32);
    }
    // The unreachable insn will send a trap back to the host which triggers the whole
    // error reporting and debug event machinary, resulting in a huge overhead ~65000 insns.
    // Make sure to scale the input size large enough to average it out.
    fe.trap();
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead: 0 }
}

// 17 insns / input
fn drop(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    for i in 0..n {
        fe.push(Operand::Const64(i as i64));
        fe.drop();
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_CONST * (n + 1);
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

// 63 insns / input
fn select(n: u64, rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    for i in 0..n {
        fe.i64_const(rng.next_u64() as i64);
        fe.i64_const(rng.next_u64() as i64);
        fe.i32_const((i % 2) as i32);
        fe.select();
        fe.drop();
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_CONST * (3 * n + 1) - INSNS_OVERHEAD_DROP * n;
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

// 13 insns / input
fn block_br_sequential(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    for _i in 0..n {
        fe.block();
        fe.br(0);
        fe.end();
    }
    fe.push(Symbol::from_str("pass"));
    let wasm = fe.finish_and_export("test").finish();
    WasmModule {
        wasm,
        overhead: INSNS_OVERHEAD_CONST,
    }
}

// 13 insns / input
fn block_br_nested(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    for _i in 0..n {
        fe.block();
    }
    for _i in 0..n {
        fe.br(0);
        fe.end();
    }
    fe.push(Symbol::from_str("pass"));
    let wasm = fe.finish_and_export("test").finish();
    WasmModule {
        wasm,
        overhead: INSNS_OVERHEAD_CONST,
    }
}

// 39 insns / input
fn br_table_nested(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    for _i in 0..n {
        fe.block();
    }
    for _i in 0..n {
        fe.i32_const(0);
        fe.br_table(&[0], 0);
        fe.end();
    }
    fe.push(Symbol::from_str("pass"));
    let wasm = fe.finish_and_export("test").finish();
    let overhead = INSNS_OVERHEAD_CONST * (n + 1);
    WasmModule { wasm, overhead }
}

// 24 insns / input
fn local_get(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 1);
    let s = fe.locals[0];
    for _i in 0..n {
        fe.local_get(s);
        fe.drop();
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_DROP * n + INSNS_OVERHEAD_CONST;
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

// 24 insns / input
fn local_set(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 1);
    let s = fe.locals[0];
    for i in 0..n {
        fe.i64_const(i as i64);
        fe.local_set(s);
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_CONST * (n + 1);
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

// 22 insns / input
fn local_tee(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 1);
    let s = fe.locals[0];
    for i in 0..n {
        fe.i64_const(i as i64);
        fe.local_tee(s);
        fe.drop();
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_CONST * (n + 1) + INSNS_OVERHEAD_DROP * n;
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

// 670 insns / input
fn call_local(n: u64, _rng: &mut StdRng) -> WasmModule {
    // a local wasm function -- the callee
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    fe.push(Symbol::from_str("pass"));
    let (m0, f0) = fe.finish();
    // the caller
    fe = m0.func(Arity(0), 0);
    for _ in 0..n {
        fe.call_func(f0);
        fe.drop();
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_DROP * n + INSNS_OVERHEAD_CONST; // overhead is only for the caller
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

// 645 insns / input
fn call_import(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut me = ModEmitter::new();
    // import the function -- the callee
    let f0 = me.import_func("t", "_", Arity(0));
    // the caller
    let mut fe = me.func(Arity(0), 0);
    for _ in 0..n {
        fe.call_func(f0);
        fe.drop();
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_DROP * n + INSNS_OVERHEAD_CONST; // overhead is only for the caller
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

// 753 insns / input
fn call_indirect(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut me = ModEmitter::new();
    // an imported function
    let f0 = me.import_func("t", "_", Arity(0));
    // a local wasm function
    let mut fe = me.func(Arity(0), 0);
    fe.push(Symbol::from_str("pass"));
    let (me, f1) = fe.finish();
    // another local wasm function
    let mut fe = me.func(Arity(0), 0);
    fe.push(Symbol::from_str("fail"));
    let (mut me, f2) = fe.finish();
    // store in table
    me.define_elems(&[f0, f1, f2]);
    let ty = me.get_fn_type(Arity(0));
    // the caller
    fe = me.func(Arity(0), 0);
    for i in 0..n {
        fe.i32_const((i % 3) as i32);
        fe.call_func_indirect(ty);
        fe.drop();
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_DROP * n + INSNS_OVERHEAD_CONST * (n + 1); // overhead is only for the caller
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

// 74 insns / input
fn global_get(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    for _ in 0..n {
        fe.global_get(GlobalRef(0));
        fe.drop();
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_DROP * n + INSNS_OVERHEAD_CONST;
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

// 88 insns / input
fn global_set(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    for i in 0..n {
        fe.i64_const(i as i64);
        fe.global_set(GlobalRef(0));
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_CONST * (n + 1);
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

fn memory_grow(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    for _ in 0..n {
        fe.i32_const(1);
        fe.memory_grow();
        fe.drop();
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_DROP * n + INSNS_OVERHEAD_CONST * (n + 1);
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

// 50 insns / input
fn memory_size(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    for _i in 0..n {
        fe.memory_size();
        fe.drop();
    }
    fe.push(Symbol::from_str("pass"));
    let overhead = INSNS_OVERHEAD_DROP * n + INSNS_OVERHEAD_CONST;
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

macro_rules! generate_i64_store_insn_code {
    ( $($func_name: ident),* )
    =>
    {
        $(
            fn $func_name(n: u64, _rng: &mut StdRng) -> WasmModule {
                let mut fe = ModEmitter::new().func(Arity(0), 0);
                for _ in 0..n {
                    fe.i32_const(0);
                    fe.i64_const(5);
                    fe.$func_name(0);
                }
                fe.push(Symbol::from_str("pass"));
                let overhead = INSNS_OVERHEAD_CONST * (2 * n + 1);
                let wasm = fe.finish_and_export("test").finish();
                WasmModule { wasm, overhead }
            }
        )*
    };
}
generate_i64_store_insn_code!(i64_store, i64_store8, i64_store16, i64_store32);

macro_rules! generate_i64_load_insn_code {
    ( $($func_name: ident),* )
    =>
    {
        $(
            fn $func_name(n: u64, _rng: &mut StdRng) -> WasmModule {
                let mut fe = ModEmitter::new().func(Arity(0), 0);
                for _ in 0..n {
                    fe.i32_const(0);
                    fe.$func_name(0);
                    fe.drop();
                }
                fe.push(Symbol::from_str("pass"));
                let overhead = INSNS_OVERHEAD_DROP * n + INSNS_OVERHEAD_CONST * (n + 1);
                let wasm = fe.finish_and_export("test").finish();
                WasmModule { wasm, overhead }
            }
        )*
    };
}
generate_i64_load_insn_code!(i64_load, i64_load8_s, i64_load16_s, i64_load32_s);

macro_rules! generate_unary_insn_code {
    ( $($func_name: ident),* )
    =>
    {
        $(
        fn $func_name(n: u64, rng: &mut StdRng) -> WasmModule {
            let mut fe = ModEmitter::new().func(Arity(0), 0);
            for _ in 0..n {
                fe.push(Operand::Const64(rng.next_u64() as i64));
                fe.$func_name();
                fe.drop();
            }
            fe.push(Symbol::from_str("pass"));
            let overhead = INSNS_OVERHEAD_DROP * n + INSNS_OVERHEAD_CONST * (n + 1);
            let wasm = fe.finish_and_export("test").finish();
            WasmModule { wasm, overhead }
        }
        )*
    };
}
generate_unary_insn_code!(i64_clz, i64_ctz, i64_popcnt, i64_eqz);

macro_rules! generate_binary_insn_code {
    ( $($func_name: ident),* )
    =>
    {
        $(
        fn $func_name(n: u64, rng: &mut StdRng) -> WasmModule {
            let mut fe = ModEmitter::new().func(Arity(0), 0);
            for _ in 0..n {
                fe.push(Operand::Const64(rng.next_u64() as i64));
                fe.push(Operand::Const64(rng.next_u64() as i64));
                fe.$func_name();
                fe.drop();
            }
            fe.push(Symbol::from_str("pass"));
            let overhead = INSNS_OVERHEAD_DROP * n + INSNS_OVERHEAD_CONST * (2 * n + 1);
            let wasm = fe.finish_and_export("test").finish();
            WasmModule { wasm, overhead }
        }
        )*
    };
}
generate_binary_insn_code!(
    i64_eq, i64_ne, i64_lt_s, i64_gt_s, i64_le_s, i64_ge_s, i64_add, i64_sub, i64_mul, i64_div_s,
    i64_rem_s, i64_and, i64_or, i64_xor, i64_shl, i64_shr_s, i64_rotl, i64_rotr
);

macro_rules! impl_wasm_insn_measure {
    ($measure: ident, $runner: ident, $wasm_gen: ident $(,$grow: literal ,$shrink: literal)? ) => {
        pub(crate) struct $measure;
        impl HostCostMeasurement for $measure {
            type Runner = $runner;
            fn new_random_case(host: &Host, rng: &mut StdRng, step: u64) -> WasmInsnSample {
                // By default we like to scale the insn count so that the actual measured work
                // averages out the overhead. If a scale factor is explictly passed in,
                // then we grow/shrink it additionally.
                let insns = step * 1000 $(* $grow / $shrink)?;
                let id: Hash = [0; 32].into();
                let module = $wasm_gen(insns, rng);
                let vm = Vm::new(&host, id, &module.wasm).unwrap();
                WasmInsnSample { vm, insns, overhead: module.overhead }
            }

            fn get_insns_overhead(_host: &Host, sample: &WasmInsnSample) -> u64 {
                sample.overhead
            }
        }
    };
}

impl_wasm_insn_measure!(WasmConstMeasure, ConstRun, push_const, 6, 1);
impl_wasm_insn_measure!(WasmDropMeasure, DropRun, drop);
impl_wasm_insn_measure!(WasmSelectMeasure, SelectRun, select);
impl_wasm_insn_measure!(WasmBrMeasure, BrRun, block_br_nested);
impl_wasm_insn_measure!(WasmBrTableMeasure, BrTableRun, br_table_nested);
impl_wasm_insn_measure!(WasmLocalGetMeasure, LocalGetRun, local_get);
impl_wasm_insn_measure!(WasmLocalSetMeasure, LocalSetRun, local_set);
impl_wasm_insn_measure!(WasmLocalTeeMeasure, LocalTeeRun, local_tee);
impl_wasm_insn_measure!(WasmCallMeasure, CallRun, call_local);
impl_wasm_insn_measure!(WasmCallIndirectMeasure, CallIndirectRun, call_indirect);
impl_wasm_insn_measure!(WasmGlobalGetMeasure, GlobalGetRun, global_get);
impl_wasm_insn_measure!(WasmGlobalSetMeasure, GlobalSetRun, global_set);
impl_wasm_insn_measure!(WasmI64StoreMeasure, I64StoreRun, i64_store);
impl_wasm_insn_measure!(WasmI64Store8Measure, I64Store8Run, i64_store8);
impl_wasm_insn_measure!(WasmI64Store16Measure, I64Store16Run, i64_store16);
impl_wasm_insn_measure!(WasmI64Store32Measure, I64Store32Run, i64_store32);
impl_wasm_insn_measure!(WasmI64LoadMeasure, I64LoadRun, i64_load);
impl_wasm_insn_measure!(WasmI64Load8Measure, I64Load8SRun, i64_load8_s);
impl_wasm_insn_measure!(WasmI64Load16Measure, I64Load16SRun, i64_load16_s);
impl_wasm_insn_measure!(WasmI64Load32Measure, I64Load32SRun, i64_load32_s);
impl_wasm_insn_measure!(WasmMemorySizeMeasure, MemorySizeRun, memory_size);
impl_wasm_insn_measure!(WasmMemoryGrowMeasure, MemoryGrowRun, memory_grow, 1, 1000);
impl_wasm_insn_measure!(WasmI64ClzMeasure, I64ClzRun, i64_clz);
impl_wasm_insn_measure!(WasmI64CtzMeasure, I64CtzRun, i64_ctz);
impl_wasm_insn_measure!(WasmI64PopcntMeasure, I64PopcntRun, i64_popcnt);
impl_wasm_insn_measure!(WasmI64EqzMeasure, I64EqzRun, i64_eqz);
impl_wasm_insn_measure!(WasmI64EqMeasure, I64EqRun, i64_eq);
impl_wasm_insn_measure!(WasmI64NeMeasure, I64NeRun, i64_ne);
impl_wasm_insn_measure!(WasmI64LtSMeasure, I64LtSRun, i64_lt_s);
impl_wasm_insn_measure!(WasmI64GtSMeasure, I64GtSRun, i64_gt_s);
impl_wasm_insn_measure!(WasmI64LeSMeasure, I64LeSRun, i64_le_s);
impl_wasm_insn_measure!(WasmI64GeSMeasure, I64GeSRun, i64_ge_s);
impl_wasm_insn_measure!(WasmI64AddMeasure, I64AddRun, i64_add);
impl_wasm_insn_measure!(WasmI64SubMeasure, I64SubRun, i64_sub);
impl_wasm_insn_measure!(WasmI64MulMeasure, I64MulRun, i64_mul);
impl_wasm_insn_measure!(WasmI64DivSMeasure, I64DivSRun, i64_div_s);
impl_wasm_insn_measure!(WasmI64RemSMeasure, I64RemSRun, i64_rem_s);
impl_wasm_insn_measure!(WasmI64AndMeasure, I64AndRun, i64_and);
impl_wasm_insn_measure!(WasmI64OrMeasure, I64OrRun, i64_or);
impl_wasm_insn_measure!(WasmI64XorMeasure, I64XorRun, i64_xor);
impl_wasm_insn_measure!(WasmI64ShlMeasure, I64ShlRun, i64_shl);
impl_wasm_insn_measure!(WasmI64ShrSMeasure, I64ShrSRun, i64_shr_s);
impl_wasm_insn_measure!(WasmI64RotlMeasure, I64RotlRun, i64_rotl);
impl_wasm_insn_measure!(WasmI64RotrMeasure, I64RotrRun, i64_rotr);

pub(crate) struct WasmInsnExecMeasure;

// This measures the cost of executing a block of WASM instructions. The
// input value is the length of the instruction block. The CPU cost should
// be linear in the length and the memory should be zero.
impl HostCostMeasurement for WasmInsnExecMeasure {
    type Runner = WasmInsnExecRun;

    fn new_random_case(host: &Host, _rng: &mut StdRng, step: u64) -> WasmInsnExecSample {
        let insns = step * 1000;
        let args = ScVec(vec![ScVal::U63(5)].try_into().unwrap());
        let id: Hash = [0; 32].into();
        let code = wasm_module_with_4n_insns(insns as usize);
        let vm = Vm::new(&host, id, &code).unwrap();
        WasmInsnExecSample { insns, args, vm }
    }
}

// Measures the cost of growing VM's linear memory. The input value is the number of pages
// to grow the memory by, where each pages is 64kB (65536). The memory cost should
// be linear and the CPU cost should be constant.
pub(crate) struct WasmMemAllocMeasure;
impl HostCostMeasurement for WasmMemAllocMeasure {
    type Runner = WasmMemAllocRun;

    fn new_random_case(
        host: &Host,
        _rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as soroban_env_host::cost_runner::CostRunner>::SampleType {
        let n_pages = input as usize;
        let id: Hash = [0; 32].into();
        let code = wasm_module_with_mem_grow(n_pages);
        let vm = Vm::new(&host, id, &code).unwrap();
        (vm, n_pages)
    }
}
