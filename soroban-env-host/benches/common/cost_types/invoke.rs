use std::rc::Rc;

use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::{InvokeHostFunctionRun, InvokeVmFunctionRun},
    xdr::Hash,
    Host, Symbol, Vm,
};
use soroban_synth_wasm::{Arity, ModEmitter};

fn wasm_module_with_empty_invoke() -> Vec<u8> {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    fe.push(Symbol::from_str("pass"));
    fe.finish_and_export("test").finish()
}

fn wasm_module_with_dummy_hostfn_invoke(repeat: u64) -> Vec<u8> {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    for _ in 0..repeat {
        fe.dummy0();
        fe.drop();
    }
    fe.push(Symbol::from_str("pass"));
    fe.finish_and_export("test").finish()
}

pub(crate) struct InvokeVmFunctionMeasure;

// This measures the roundtrip cost of invoking a VM (guest) function from the host.
// The guest function receives 0 args and does no-op. Both the CPU and mem cost should
// be constant.
impl HostCostMeasurement for InvokeVmFunctionMeasure {
    type Runner = InvokeVmFunctionRun;

    fn new_random_case(host: &Host, _rng: &mut StdRng, _input: u64) -> Rc<Vm> {
        let id: Hash = [0; 32].into();
        let code = wasm_module_with_empty_invoke();
        Vm::new(&host, id, &code).unwrap()
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

    fn new_random_case(host: &Host, _rng: &mut StdRng, input: u64) -> Rc<Vm> {
        let input = input * 1000;
        let id: Hash = [0; 32].into();
        let code = wasm_module_with_dummy_hostfn_invoke(input);
        Vm::new(&host, id, &code).unwrap()
    }
}
