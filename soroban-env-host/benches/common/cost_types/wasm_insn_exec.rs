use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::{WasmInsnExecRun, WasmInsnExecSample, WasmMemAllocRun},
    xdr::{Hash, ScVal, ScVec},
    Host, Symbol, Vm,
};
use soroban_synth_wasm::{Arity, LocalRef, ModEmitter, Operand};

fn wasm_module_with_4n_insns(n: usize) -> Vec<u8> {
    let mut fe = ModEmitter::new().func(Arity(1), 0);
    let arg = fe.args[0];
    fe.push(Operand::Const64(1));
    for i in 0..n {
        fe.push(arg);
        fe.push(Operand::Const64(i as i64));
        fe.mul64();
        fe.add64();
    }
    fe.drop();
    fe.push(Symbol::from_str("pass"));
    fe.finish_and_export("test").finish()
}

fn wasm_module_with_mem_grow(n_pages: usize) -> Vec<u8> {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    fe.push(Operand::Const32(n_pages as i32));
    fe.mem_grow(LocalRef(0));
    // need to drop the return value on the stack because it's an i32
    // and the function needs an i64 return value.
    fe.drop();
    fe.push(Symbol::from_str("pass"));
    fe.finish_and_export("test").finish()
}

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
