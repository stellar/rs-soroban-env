use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_host::{cost_runner::*, xdr::Hash, Host, Symbol, Vm};
use soroban_synth_wasm::{Arity, GlobalRef, ModEmitter, Operand};
use wasm_encoder::{ConstExpr, ExportKind, ValType};

// These are fp numbers to minimize rounding during overhead calculation.
// The fact they both turned out to be "whole" numbers is pure luck.
const INSNS_OVERHEAD_CONST: f64 = 10.0; // measured by `push_const`
const INSNS_OVERHEAD_DROP: f64 = 3.0; // 17; // measured by `drop`

struct WasmModule {
    wasm: Vec<u8>,
    overhead: u64,
}

// ModEmitter's default constructors are a little too spartan for our needs, we
// want our benchmarks to all have at least one imported function and at least
// one defined and exported function, so we're in the right performance tier.
// But we also don't want to go changing those constructors since it'll perturb
// a lot of non-benchmark users.
trait ModEmitterExt {
    fn bench_default() -> Self;
    fn bench_from_configs(mem_pages: u32, elem_count: u32) -> Self;
    fn add_bench_import(self) -> Self;
    fn add_bench_export(self) -> Self;
    fn add_bench_baseline_material(self) -> Self;
}

impl ModEmitterExt for ModEmitter {
    fn add_bench_import(mut self) -> Self {
        self.import_func("t", "_", Arity(0));
        self
    }
    fn add_bench_export(self) -> Self {
        let mut fe = self.func(Arity(0), 0);
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        fe.finish_and_export("default")
    }
    fn add_bench_baseline_material(self) -> Self {
        self.add_bench_import().add_bench_export()
    }

    fn bench_default() -> Self {
        Self::add_bench_baseline_material(ModEmitter::default())
    }

    fn bench_from_configs(mem_pages: u32, elem_count: u32) -> Self {
        Self::add_bench_baseline_material(ModEmitter::from_configs(mem_pages, elem_count))
    }
}

pub fn wasm_module_with_n_internal_funcs(n: usize) -> Vec<u8> {
    let mut me = ModEmitter::bench_default();
    for _ in 0..n {
        let mut fe = me.func(Arity(0), 0);
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        (me, _) = fe.finish();
    }
    me.finish()
}

pub fn wasm_module_with_n_insns(n: usize) -> Vec<u8> {
    // We actually emit 4 instructions per loop iteration, so we need to divide by 4.
    let n = 1 + (n / 4);
    let mut fe = ModEmitter::bench_default().func(Arity(1), 0);
    let arg = fe.args[0];
    fe.push(Operand::Const64(1));
    for i in 0..n {
        fe.push(arg.0);
        fe.push(Operand::Const64(i as i64));
        fe.i64_mul();
        fe.i64_add();
    }
    fe.drop();
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    fe.finish_and_export("test").finish()
}
pub fn wasm_module_with_n_globals(n: usize) -> Vec<u8> {
    let mut me = ModEmitter::bench_default();
    for i in 0..n {
        me.global(ValType::I64, true, &ConstExpr::i64_const(i as i64));
    }
    me.finish()
}

pub fn wasm_module_with_n_imports(n: usize) -> Vec<u8> {
    let mut me = ModEmitter::default().add_bench_import();
    let names = Vm::get_all_host_functions();
    for (module, name, arity) in names.iter().take(n) {
        if *module == "t" {
            continue;
        }
        me.import_func(module, name, Arity(*arity));
    }
    me.add_bench_export().finish()
}

pub fn wasm_module_with_n_exports(n: usize) -> Vec<u8> {
    let me = ModEmitter::bench_default();
    let mut fe = me.func(Arity(0), 0);
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let (mut me, fid) = fe.finish();
    for i in 0..n {
        me.export(format!("_{i}").as_str(), ExportKind::Func, fid.0);
    }
    me.finish()
}

pub fn wasm_module_with_n_table_entries(n: usize) -> Vec<u8> {
    let me = ModEmitter::bench_from_configs(1, n as u32);
    let mut fe = me.func(Arity(0), 0);
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let (mut me, f) = fe.finish();
    let funcs = vec![f; n];
    me.define_elem_funcs(&funcs);
    me.finish()
}

pub fn wasm_module_with_n_types(mut n: usize) -> Vec<u8> {
    let mut me = ModEmitter::bench_default();
    // There's a max of 1,000,000 types, so we just make a loop
    // that covers more than that many combinations, and break when we've got
    // to the requested number.
    let val_types = &[ValType::I32, ValType::I64];

    'top: for a in val_types {
        for b in val_types {
            for c in val_types {
                for d in val_types {
                    for e in val_types {
                        for f in val_types {
                            for g in val_types {
                                for h in val_types {
                                    for i in val_types {
                                        for j in val_types {
                                            for aa in val_types {
                                                for bb in val_types {
                                                    for cc in val_types {
                                                        for dd in val_types {
                                                            for ee in val_types {
                                                                for ff in val_types {
                                                                    for gg in val_types {
                                                                        for hh in val_types {
                                                                            for ii in val_types {
                                                                                for jj in val_types
                                                                                {
                                                                                    if n == 0 {
                                                                                        break 'top;
                                                                                    }
                                                                                    n -= 1;
                                                                                    let params = &[
                                                                                        *a, *b, *c,
                                                                                        *d, *e, *f,
                                                                                        *g, *h, *i,
                                                                                        *j, *aa,
                                                                                        *bb, *cc,
                                                                                        *dd, *ee,
                                                                                        *ff, *gg,
                                                                                        *hh, *ii,
                                                                                        *jj,
                                                                                    ];
                                                                                    me.add_raw_fn_type(params, &[]);
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    me.finish()
}

pub fn wasm_module_with_n_elem_segments(n: usize) -> Vec<u8> {
    let me = ModEmitter::bench_from_configs(1, n as u32);
    let mut fe = me.func(Arity(0), 0);
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let (mut me, f) = fe.finish();
    for _ in 0..n {
        me.define_elem_funcs(&[f]);
    }
    me.finish()
}

pub fn wasm_module_with_n_data_segments(n: usize) -> Vec<u8> {
    let mem_offset = n as u32 * 1024;
    let mut me = ModEmitter::bench_from_configs(1 + mem_offset / 65536, 0);
    for _ in 0..n {
        me.define_data_segment(n as u32 * 1024, vec![1, 2, 3, 4]);
    }
    me.finish()
}

pub fn wasm_module_with_n_data_segment_bytes(n: usize) -> Vec<u8> {
    let mut me = ModEmitter::bench_from_configs(1 + (n / 0x10000) as u32, 0);
    me.define_data_segment(0, vec![0xff; n]);
    me.finish()
}

fn wasm_module_with_mem_grow(n_pages: usize) -> Vec<u8> {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    fe.push(Operand::Const32(n_pages as i32));
    fe.memory_grow();
    // need to drop the return value on the stack because it's an i32
    // and the function needs an i64 return value.
    fe.drop();
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    fe.finish_and_export("test").finish()
}

// A Wasm module with a single const to serve as the baseline
fn wasm_module_baseline_pass() -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead: 0 }
}

// A Wasm module with a single trap to serve as the baseline
fn wasm_module_baseline_trap() -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    fe.trap();
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead: 0 }
}

fn push_const(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    for i in 0..n {
        fe.i32_const(i as i32);
    }
    fe.trap();
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead: 0 }
}

// The last fe.push(Const) is not counted as overhead since it's already been included
// in the baseline. This applies to all following instruction generators.
fn drop(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    for i in 0..n {
        fe.push(Operand::Const64(i as i64));
        fe.drop();
    }
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let overhead = (INSNS_OVERHEAD_CONST * n as f64) as u64;
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

fn select(n: u64, rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    for i in 0..n {
        fe.i64_const(rng.next_u64() as i64);
        fe.i64_const(rng.next_u64() as i64);
        fe.i32_const((i % 2) as i32);
        fe.select();
        fe.drop();
    }
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let overhead = INSNS_OVERHEAD_CONST * (3.0 * n as f64) - INSNS_OVERHEAD_DROP * n as f64;
    let wasm = fe.finish_and_export("test").finish();
    WasmModule {
        wasm,
        overhead: overhead as u64,
    }
}

fn block_br_sequential(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    for _i in 0..n {
        fe.block();
        fe.br(0);
        fe.end();
    }
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead: 0 }
}

fn block_br_nested(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    for _i in 0..n {
        fe.block();
    }
    for _i in 0..n {
        fe.br(0);
        fe.end();
    }
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead: 0 }
}

fn br_table_nested(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    for _i in 0..n {
        fe.block();
    }
    for _i in 0..n {
        fe.i32_const(0);
        fe.br_table(&[0], 0);
        fe.end();
    }
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let wasm = fe.finish_and_export("test").finish();
    let overhead = (INSNS_OVERHEAD_CONST * n as f64) as u64;
    WasmModule { wasm, overhead }
}

fn local_get(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 1);
    let s = fe.locals[0].0;
    for _i in 0..n {
        fe.local_get(s);
    }
    fe.trap();
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead: 0 }
}

fn local_set(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 1);
    let s = fe.locals[0].0;
    for i in 0..n {
        fe.i64_const(i as i64);
        fe.local_set(s);
    }
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let overhead = (INSNS_OVERHEAD_CONST * n as f64) as u64;
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

fn local_tee(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 1);
    let s = fe.locals[0].0;
    for i in 0..n {
        fe.i64_const(i as i64);
        fe.local_tee(s);
        fe.drop();
    }
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let overhead = INSNS_OVERHEAD_CONST * n as f64 + INSNS_OVERHEAD_DROP * n as f64;
    let wasm = fe.finish_and_export("test").finish();
    WasmModule {
        wasm,
        overhead: overhead as u64,
    }
}

fn call_local(n: u64, _rng: &mut StdRng) -> WasmModule {
    // a local wasm function -- the callee
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let (m0, f0) = fe.finish();
    // the caller
    fe = m0.func(Arity(0), 0);
    for _ in 0..n {
        fe.call_func(f0);
    }
    fe.trap();
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead: 0 }
}

fn call_import(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut me = ModEmitter::default();
    // import the function -- the callee
    let f0 = me.import_func("t", "_", Arity(0));
    // the caller
    let mut fe = me.func(Arity(0), 0);
    for _ in 0..n {
        fe.call_func(f0);
    }
    fe.trap();
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead: 0 }
}

fn call_indirect(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut me = ModEmitter::default();
    // an imported function
    let f0 = me.import_func("t", "_", Arity(0));
    // a local wasm function
    let mut fe = me.func(Arity(0), 0);
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let (me, f1) = fe.finish();
    // another local wasm function
    let mut fe = me.func(Arity(0), 0);
    fe.push(Symbol::try_from_small_str("fail").unwrap());
    let (mut me, f2) = fe.finish();
    // store in table
    me.define_elem_funcs(&[f0, f1, f2]);
    let ty = me.get_fn_type(Arity(0), Arity(1));
    // the caller
    fe = me.func(Arity(0), 0);
    for i in 0..n {
        fe.i32_const((i % 3) as i32);
        fe.call_func_indirect(ty);
        fe.drop();
    }
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let overhead = INSNS_OVERHEAD_DROP * n as f64 + INSNS_OVERHEAD_CONST * n as f64; // overhead is only for the caller
    let wasm = fe.finish_and_export("test").finish();
    WasmModule {
        wasm,
        overhead: overhead as u64,
    }
}

fn global_get(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    for _ in 0..n {
        fe.global_get(GlobalRef(0));
    }
    fe.trap();
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead: 0 }
}

fn global_set(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    for i in 0..n {
        fe.i64_const(i as i64);
        fe.global_set(GlobalRef(0));
    }
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let overhead = (INSNS_OVERHEAD_CONST * n as f64) as u64;
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead }
}

fn memory_grow(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    for _ in 0..n {
        fe.i32_const(1);
        fe.memory_grow();
        // memory_grow returns number of new pages, here we drop the value
        fe.drop();
    }
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    let overhead = INSNS_OVERHEAD_DROP * n as f64 + INSNS_OVERHEAD_CONST * n as f64;
    let wasm = fe.finish_and_export("test").finish();
    WasmModule {
        wasm,
        overhead: overhead as u64,
    }
}

fn memory_size(n: u64, _rng: &mut StdRng) -> WasmModule {
    let mut fe = ModEmitter::default().func(Arity(0), 0);
    for _i in 0..n {
        fe.memory_size();
    }
    fe.trap();
    let wasm = fe.finish_and_export("test").finish();
    WasmModule { wasm, overhead: 0 }
}

macro_rules! generate_i64_store_insn_code {
    ( $($func_name: ident),* )
    =>
    {
        $(
            fn $func_name(n: u64, _rng: &mut StdRng) -> WasmModule {
                let mut fe = ModEmitter::default().func(Arity(0), 0);
                for _ in 0..n {
                    fe.i32_const(0);
                    fe.i64_const(5);
                    fe.$func_name(0, 0);
                }
                fe.push(Symbol::try_from_small_str("pass").unwrap());
                let overhead = INSNS_OVERHEAD_CONST * (2 * n) as f64;
                let wasm = fe.finish_and_export("test").finish();
                WasmModule { wasm, overhead: overhead as u64 }
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
                let mut fe = ModEmitter::default().func(Arity(0), 0);
                for _ in 0..n {
                    fe.i32_const(0);
                    fe.$func_name(0, 0);
                }
                fe.trap();
                let overhead = INSNS_OVERHEAD_CONST * n as f64;
                let wasm = fe.finish_and_export("test").finish();
                WasmModule { wasm, overhead: overhead as u64 }
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
            let mut fe = ModEmitter::default().func(Arity(0), 0);
            for _ in 0..n {
                fe.push(Operand::Const64(rng.next_u64() as i64));
                fe.$func_name();
                fe.drop();
            }
            fe.push(Symbol::try_from_small_str("pass").unwrap());
            let overhead = INSNS_OVERHEAD_DROP * n as f64 + INSNS_OVERHEAD_CONST * n as f64;
            let wasm = fe.finish_and_export("test").finish();
            WasmModule { wasm, overhead: overhead as u64 }
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
            let mut fe = ModEmitter::default().func(Arity(0), 0);
            for _ in 0..n {
                fe.push(Operand::Const64(rng.next_u64() as i64));
                fe.push(Operand::Const64(rng.next_u64() as i64));
                fe.$func_name();
                fe.drop();
            }
            fe.push(Symbol::try_from_small_str("pass").unwrap());
            let overhead = INSNS_OVERHEAD_DROP * n as f64 + INSNS_OVERHEAD_CONST * (2 * n) as f64;
            let wasm = fe.finish_and_export("test").finish();
            WasmModule { wasm, overhead: overhead as u64 }
        }
        )*
    };
}
generate_binary_insn_code!(
    i64_eq, i64_ne, i64_lt_s, i64_gt_s, i64_le_s, i64_ge_s, i64_add, i64_sub, i64_mul, i64_div_s,
    i64_rem_s, i64_and, i64_or, i64_xor, i64_shl, i64_shr_s, i64_rotl, i64_rotr
);

macro_rules! impl_wasm_insn_measure_with_baseline_trap {
    ($measure: ident, $runner: ident, $wasm_gen: ident) => {
        pub(crate) struct $measure;
        impl HostCostMeasurement for $measure {
            type Runner = $runner;
            fn new_random_case(host: &Host, rng: &mut StdRng, step: u64) -> WasmInsnSample {
                let insns = 1 + step * Self::STEP_SIZE;
                let id: Hash = [0; 32].into();
                let module = $wasm_gen(insns, rng);
                let vm = Vm::new(&host, id, &module.wasm).unwrap();
                WasmInsnSample {
                    vm,
                    insns,
                    overhead: module.overhead,
                }
            }

            fn new_baseline_case(host: &Host, _rng: &mut StdRng) -> WasmInsnSample {
                let module = wasm_module_baseline_trap();
                let id: Hash = [0; 32].into();
                let vm = Vm::new(&host, id, &module.wasm).unwrap();
                WasmInsnSample {
                    vm,
                    insns: 0,
                    overhead: module.overhead,
                }
            }

            fn get_insns_overhead_per_sample(_host: &Host, sample: &WasmInsnSample) -> u64 {
                sample.overhead
            }
        }
    };
}

macro_rules! impl_wasm_insn_measure_with_baseline_pass {
    ($measure: ident, $runner: ident, $wasm_gen: ident $(,$grow: literal ,$shrink: literal)? ) => {
        pub(crate) struct $measure;
        impl HostCostMeasurement for $measure {
            type Runner = $runner;
            fn new_random_case(host: &Host, rng: &mut StdRng, step: u64) -> WasmInsnSample {
                // By default we like to scale the insn count so that the actual measured work
                // averages out the overhead. If a scale factor is explictly passed in,
                // then we grow/shrink it additionally.
                let insns = 1 + step * Self::STEP_SIZE $(* $grow / $shrink)?;
                let id: Hash = [0; 32].into();
                let module = $wasm_gen(insns, rng);
                let vm = Vm::new(&host, id, &module.wasm).unwrap();
                WasmInsnSample { vm, insns, overhead: module.overhead }
            }

            fn new_baseline_case(host: &Host, _rng: &mut StdRng) -> WasmInsnSample {
                let module = wasm_module_baseline_pass();
                let id: Hash = [0; 32].into();
                let vm = Vm::new(&host, id, &module.wasm).unwrap();
                WasmInsnSample { vm, insns: 0, overhead: module.overhead }
            }

            fn get_insns_overhead_per_sample(_host: &Host, sample: &WasmInsnSample) -> u64 {
                sample.overhead
            }
        }
    };
}

// The `WasmInsnExec` run has been replaced by individual Wasm instruction measurements below.
// The wasm instructions are catagorized into one of the `WasmInsnTier`. The `BASE` tier wasm
// insn by definition costs 1 fuel. Other tiers `ENTITY`, `LOAD`, `STORE`, `CALL` may cost more
// fuels. The goal of wasm_insn_measure is to 1. obtain the average cpu cost of a BASE tier insn
// (i.e. per fuel) and 2. the relative cost ratio of various tiers which determines the FuelConfig
// (will serve as input to the `wasmi::Engine`).

// The implementation is divided into two catagories based on how baseline cost is captured
// and eliminated. A Wasm file cannot contain just the instruction we are interested in measuring,
// it must contain setup code. To deduce the cost of the target instruction, we need to measure
// and eliminate the baseline.

// These below uses a trapping baseline. The code typically contains a bunch of target instructions
// plus a trap. Trap exists such that it doesn't need to clear the stack (by e.g. adding more `drop`s)
// The trap cost is constant although it is a bit large and sometimes noisy, so its effect can be
// averaged out over large number of target instructions.
impl_wasm_insn_measure_with_baseline_trap!(WasmConstMeasure, ConstRun, push_const);
impl_wasm_insn_measure_with_baseline_trap!(WasmLocalGetMeasure, LocalGetRun, local_get);
impl_wasm_insn_measure_with_baseline_trap!(WasmCallLocalMeasure, CallLocalRun, call_local);
impl_wasm_insn_measure_with_baseline_trap!(WasmCallImportMeasure, CallImportRun, call_import);
impl_wasm_insn_measure_with_baseline_trap!(WasmGlobalGetMeasure, GlobalGetRun, global_get);
impl_wasm_insn_measure_with_baseline_trap!(WasmI64LoadMeasure, I64LoadRun, i64_load);
impl_wasm_insn_measure_with_baseline_trap!(WasmI64Load8Measure, I64Load8SRun, i64_load8_s);
impl_wasm_insn_measure_with_baseline_trap!(WasmI64Load16Measure, I64Load16SRun, i64_load16_s);
impl_wasm_insn_measure_with_baseline_trap!(WasmI64Load32Measure, I64Load32SRun, i64_load32_s);
impl_wasm_insn_measure_with_baseline_trap!(WasmMemorySizeMeasure, MemorySizeRun, memory_size);

// These below uses a passing baseline. They normally require clearing the stack or setting it up for
// the next iteration. To subtract the baseline requires using prefitted baseline instruction costs
// -- `Const` and `Drop`. It's an iterative process: Fit Const --> use Const to fit `Drop`
// --> Use both of them as the baseline cost of other measurements.
impl_wasm_insn_measure_with_baseline_pass!(WasmDropMeasure, DropRun, drop);
impl_wasm_insn_measure_with_baseline_pass!(WasmSelectMeasure, SelectRun, select);
impl_wasm_insn_measure_with_baseline_pass!(WasmBrMeasure, BrRun, block_br_nested);
impl_wasm_insn_measure_with_baseline_pass!(WasmBrTableMeasure, BrTableRun, br_table_nested);
impl_wasm_insn_measure_with_baseline_pass!(WasmLocalSetMeasure, LocalSetRun, local_set);
impl_wasm_insn_measure_with_baseline_pass!(WasmLocalTeeMeasure, LocalTeeRun, local_tee);
impl_wasm_insn_measure_with_baseline_pass!(WasmCallIndirectMeasure, CallIndirectRun, call_indirect);
impl_wasm_insn_measure_with_baseline_pass!(WasmGlobalSetMeasure, GlobalSetRun, global_set);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64StoreMeasure, I64StoreRun, i64_store);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64Store8Measure, I64Store8Run, i64_store8);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64Store16Measure, I64Store16Run, i64_store16);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64Store32Measure, I64Store32Run, i64_store32);
// Note this is running the `WasmInsnExec` (cpu) cost of a MemeoryGrow operation.
// The iter/input corresponds to the cpu model. Also, Wasmi internally compensates
// extra fuels for memory operations (1 fuel per 64 bytes), so the iter/input
// will be different from an expected "normal" operation.
impl_wasm_insn_measure_with_baseline_pass!(
    WasmMemoryGrowMeasure,
    MemoryGrowRun,
    memory_grow,
    1,
    1000
);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64ClzMeasure, I64ClzRun, i64_clz);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64CtzMeasure, I64CtzRun, i64_ctz);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64PopcntMeasure, I64PopcntRun, i64_popcnt);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64EqzMeasure, I64EqzRun, i64_eqz);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64EqMeasure, I64EqRun, i64_eq);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64NeMeasure, I64NeRun, i64_ne);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64LtSMeasure, I64LtSRun, i64_lt_s);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64GtSMeasure, I64GtSRun, i64_gt_s);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64LeSMeasure, I64LeSRun, i64_le_s);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64GeSMeasure, I64GeSRun, i64_ge_s);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64AddMeasure, I64AddRun, i64_add);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64SubMeasure, I64SubRun, i64_sub);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64MulMeasure, I64MulRun, i64_mul);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64DivSMeasure, I64DivSRun, i64_div_s);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64RemSMeasure, I64RemSRun, i64_rem_s);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64AndMeasure, I64AndRun, i64_and);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64OrMeasure, I64OrRun, i64_or);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64XorMeasure, I64XorRun, i64_xor);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64ShlMeasure, I64ShlRun, i64_shl);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64ShrSMeasure, I64ShrSRun, i64_shr_s);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64RotlMeasure, I64RotlRun, i64_rotl);
impl_wasm_insn_measure_with_baseline_pass!(WasmI64RotrMeasure, I64RotrRun, i64_rotr);

// Note we no longer run the `WasmMemAllocRun` because memory metering has been moved entirely
// to inside wasmi, with a 1-to-1 correspondence between 1 memory fuel and 1 host byte, assuming
// `WasmMemAlloc` measures the growth of the linear memory via `memory_grow` operation.
// There is no point in doing any host calibration, otherwise we will be measuring noise of
// the internal memory allocator usage of the wasmi interpreter.
