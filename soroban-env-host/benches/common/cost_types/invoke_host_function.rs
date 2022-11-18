use std::rc::Rc;

use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::{cost_runner::InvokeHostFunctionRun, xdr::Hash, Host, Vm};
use soroban_synth_wasm::{Arity, ModEmitter};

fn wasm_module_with_invoke_import() -> Vec<u8> {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    fe.dummy0();
    fe.finish_and_export("test").finish()
}

pub(crate) struct InvokeHostFunctionMeasure;

// This measures the roundtrip cost of invoking a host function: host->Vm->host, where
// the host function receives 0 args and does no-op. Both the CPU and mem cost should
// be constant. The cost changes very little wrt the number of input args to the host
// function: 2,663 (0 arg) vs 2,698 (3 args), therefore we use 0 arg func for simplicity.
impl HostCostMeasurement for InvokeHostFunctionMeasure {
    type Runner = InvokeHostFunctionRun;

    fn new_random_case(host: &Host, _rng: &mut StdRng, _input: u64) -> Rc<Vm> {
        let id: Hash = [0; 32].into();
        let code = wasm_module_with_invoke_import();
        Vm::new(&host, id, &code).unwrap()
    }
}
