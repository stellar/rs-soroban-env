use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::budget::CostType;
use soroban_env_host::xdr::{Hash, ScVal, ScVec};
use soroban_env_host::{Host, Vm};
use soroban_synth_wasm::{Arity, ModEmitter, Operand};
use std::rc::Rc;

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
    fe.push(1u32);
    fe.finish_and_export("test").finish()
}

pub(crate) struct WasmInsnExecRun {
    insns: u64,
    args: ScVec,
    vm: Rc<Vm>,
}

// This measures the cost of executing a block of WASM instructions. The
// input value is the length of the instruction block. The CPU cost should
// be linear in the length and the memory should be zero.
impl HostCostMeasurement for WasmInsnExecRun {
    const COST_TYPE: CostType = CostType::WasmInsnExec;

    fn new_random_case(host: &Host, _rng: &mut StdRng, step: u64) -> Self {
        let insns = step * 1000;
        let args = ScVec(vec![ScVal::U63(5)].try_into().unwrap());
        let id: Hash = [0; 32].into();
        let code = wasm_module_with_4n_insns(insns as usize);
        let vm = Vm::new(&host, id, &code).unwrap();
        Self { insns, args, vm }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.insns * 4
    }

    fn run(&mut self, _iter: u64, host: &Host) {
        let scval = self.vm.invoke_function(host, "test", &self.args).unwrap();
        assert_eq!(scval, ScVal::Symbol("pass".try_into().unwrap()));
    }
}
