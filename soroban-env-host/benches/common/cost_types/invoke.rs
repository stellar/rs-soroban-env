use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{
    cost_runner::{InvokeHostFunctionRun, InvokeVmFunctionRun},
    xdr::Hash,
    Host, Symbol, Vm,
};
use soroban_synth_wasm::{Arity, ModEmitter};
use std::rc::Rc;
use wasmi::Value;

const MAX_VM_ARGS: usize = 32;

fn wasm_module_with_empty_invoke() -> Vec<u8> {
    let mut fe = ModEmitter::default().func(Arity(MAX_VM_ARGS as u32), 0);
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    fe.finish_and_export("test").finish()
}

pub(crate) struct InvokeVmFunctionMeasure;

// This measures the roundtrip cost of invoking a VM (guest) function from the host.
// The guest function receives 0 args and does no-op. Both the CPU and mem cost should
// be constant.
impl HostCostMeasurement for InvokeVmFunctionMeasure {
    type Runner = InvokeVmFunctionRun;

    fn new_random_case(host: &Host, _rng: &mut StdRng, _input: u64) -> (Rc<Vm>, Vec<Value>) {
        let id: Hash = [0; 32].into();
        let code = wasm_module_with_empty_invoke();
        let vm = Vm::new(&host, id, &code).unwrap();
        let args = vec![Value::I64(0); MAX_VM_ARGS];
        (vm, args)
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
