use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::{WasmInsnExecRun, WasmInsnExecSample},
    xdr::{Hash, ScVal, ScVec},
    Host, Symbol, Vm,
};
use soroban_synth_wasm::{Arity, ModEmitter, Operand};

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
