use super::{
    wasm_module_with_empty_invoke, wasm_module_with_m_fns_each_with_n_insns_returning_early,
};
use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::{CostRunner, InvokeHostFunctionRun, InvokeVmFunctionMode, InvokeVmFunctionRun},
    xdr::Hash,
    Host, Vm,
};

use std::rc::Rc;
use wasmi::Val as Value;

pub(crate) struct InvokeVmFunctionMeasure;

// This measures the roundtrip cost of invoking a VM (guest) function from the
// host. The guest function receives max args and pushes a trivial result.
//
// There are two fundamentally different modes this benchmark runs -- it is a
// bit special and unlike other benchmarks.
//
// The first way it runs is `InvokeVmFunctionMode::Normal`: it builds a single
// small function and does repeated invocations of it. This is the default
// behaviour and does what you'd expect: it measures the cost of invoking a
// function on its own. The small function does trivial work (pushes a constant
// return value) and returns. This mode ignores the `input`` parameter (because
// InvokeVmFunction is a constant-cost operation, not a variable-cost one).
//
// The second way it runs is `InvokeVmFunctionMode::CheckLazyCompilationCosts`:
// it builds a module with as many functions as the cost runner's
// `RUN_ITERATIONS` value, and varies the number of instructions in all those
// functions by the benchmark's `input` parameter. This is a special mode
// triggered by the env var `CHECK_LAZY_COMPILATION_COSTS=1` that helps us
// capture the cost of wasmi's lazy compilation of functions, and check that
// it's approximately what we expect (spoiler: it is).
//
// In particular, as background information:
//
//    - Most of our other calibrations show approximately 8-16 CPU instructions
//      per wasm instruction.
//    - Wasmi charges generally 1 fuel per wasm instruction (unless it's a
//      bigger instruction). See calls to push_fueled_instr in wasmi.
//    - Wasmi has a hard-wired meaning for a `fuel`: it's the cost of moving 8
//      registers or copying 64 bytes. See FuelCosts::default in wasmi.
//    - Wasmi claims a general performance estimate of 8-16x slower than native
//      in 0.32+ benchmarks https://wasmi-labs.github.io/blog/posts/wasmi-v0.32/
//    - So for the purposes of gut-checking wasmi, we'll say a fuel is
//      approximately 8-16 CPU instructions (as of the 0.32 register machine).
//
// Given that background, we now turn our attention to (lazy) compilation costs:
//
//    - Wasmi charges _itself_ the cost of compilation, in fuel units.
//    - Its compilation cost is hard-wired at 9 fuel per byte of wasm code.
//      See wasmi InternalFuncEntity::compile.
//    - We observe, when running this benchmark, compilation-cost benchmarks
//      approximately like so:
//
//        wasm insns | bytes   | fuel    |   CPU
//        -----------+---------+---------+----------
//            1,000  |  1,700  |  15,000 |   244,800
//            2,000  |  3,400  |  30,600 |   489,600
//            ...
//           10,000  | 17,000  | 150,000 | 2,448,000
//
//    - In other words, when _measuring_ the cost of compilation (in this
//      benchmark) we observe costs broadly in line with expectations: about
//      1.7 bytes per wasm instruction, being charged 1.7*9=15 fuel per
//      instruction, and costing about 16 CPU instructions per fuel, or about
//      16*15 = 240 CPU instructions per wasm instruction compiled.
//
// This is all _extremely_ rough. We don't know the number of wasm instructions
// in a given function, nor their mix of types, nor the costs of other variable
// factors like the number of locals or control-flow nesting, so both the
// variable size of the function in bytes _and_ the (likely highly-variable)
// cost of compiling different instructions is glossed over.
//
// Moreover, an attentive reader will also see that in the transition to wasmi
// 0.36 we've actually lost the notion of different cost-classes of wasmi
// instructions altogether, so all our per-tier cost calibration is not
// meaningful anymore. We don't even load a variety of
// wasmi-fuel-to-our-CPU-instructions ratios anymore.
//
// Finally, it's worth noting why this is a special mode of a benchmark rather
// than a different benchmark: it's because we're not actually taking the output
// of this mode and _using_ it as a cost center. We don't charge for lazy
// compilation, and there's no associated cost center that takes "size of a
// function" as an input parameter (it wouldn't be approriate to do that on
// InvokeVmFunction, since the 2nd-and-after invocations to a function _don't_
// recompile it, only the first). All we're doing in this weird secondary mode
// is confirming that the cost of lazy compilation that _wasmi is charging
// itself_, through its fuel system, is approximately what we expect. And it is.
// So this makes us a bit more comfortable delegating accounting-for-compilation
// to it!

impl HostCostMeasurement for InvokeVmFunctionMeasure {
    type Runner = InvokeVmFunctionRun;

    fn new_random_case(
        host: &Host,
        _rng: &mut StdRng,
        input: u64,
    ) -> (Rc<Vm>, Vec<Value>, InvokeVmFunctionMode) {
        let id: Hash = [0; 32].into();
        let (code, mode) = if std::env::var("CHECK_LAZY_COMPILATION_COSTS").is_ok() {
            // We're going to compile a module with a bunch of functions, each of
            // which we're going to invoke _once_ in order to trigger lazy
            // compilation of it and then move on to the next one.
            let m_funcs = <InvokeVmFunctionRun as CostRunner>::RUN_ITERATIONS as usize;

            // We're going to vary the number of instructions in each function
            // by the input parameter. This will let us see how the cost of lazy
            // compilation scales (hopefully linearly!) with the size of the
            // function.
            let n_insns = input as usize * 1000;

            let code = wasm_module_with_m_fns_each_with_n_insns_returning_early(m_funcs, n_insns);

            // As we are not trying to integrate with the normal benchmark /
            // calibration / slope-fitting system (see explanation above) we
            // just _print out_ the relevant inputs here; this can be matched up
            // with the CPU measurements printed immediately after by the rest
            // of the benchmark system.
            let wasmi_compilation_cost_in_fuel_per_byte = 9;
            let code_len_per_fn = code.len() / m_funcs;
            let fuel_per_fn = code_len_per_fn * wasmi_compilation_cost_in_fuel_per_byte;
            eprintln!(
                "[CHECK_LAZY_COMPILATION_COSTS] {} wasm insns per fn, {} bytes, {} fuel",
                n_insns, code_len_per_fn, fuel_per_fn
            );

            (code, InvokeVmFunctionMode::CheckLazyCompilationCosts)
        } else {
            (
                wasm_module_with_empty_invoke(),
                InvokeVmFunctionMode::Normal,
            )
        };
        let vm = Vm::new(&host, id, &code).unwrap();
        let args = vec![Value::I64(0); Vm::MAX_VM_ARGS];
        (vm, args, mode)
    }
}

pub(crate) struct InvokeHostFunctionMeasure;

// Measures the cost of host function invocation inside an VM.
// The wasm code repeatly calls an empty host function. Input is number of Vm->host invocations,
// the cpu cost should be linear wrt the input and mem cost should be constant.
// The setup here also includes an initial host->Vm call which has `CostType: InvokeVmFunction`.
// We therefore use a large input such that its cost is averaged out.
impl HostCostMeasurement for InvokeHostFunctionMeasure {
    type Runner = InvokeHostFunctionRun;

    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> Rc<Vm> {
        InvokeVmFunctionMeasure::new_random_case(host, rng, input).0
    }
}
