use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, Rng};
use soroban_env_host::{
    cost_runner::{VmInstantiationRun, VmInstantiationSample},
    xdr, Host,
};

pub(crate) struct VmInstantiationMeasure;

// This measures the cost of instantiating a host::Vm on a variety of possible
// wasm modules, of different sizes. The input value should be the size of the
// module, though for now we're just selecting modules from the fixed example
// repertoire. Costs should be linear.
impl HostCostMeasurement for VmInstantiationMeasure {
    type Runner = VmInstantiationRun;

    fn new_best_case(_host: &Host, _rng: &mut StdRng) -> VmInstantiationSample {
        let id: xdr::Hash = [0; 32].into();
        let wasm: Vec<u8> = soroban_test_wasms::ADD_I32.clone().into();
        VmInstantiationSample { id, wasm }
    }

    fn new_worst_case(_host: &Host, _rng: &mut StdRng, _input: u64) -> VmInstantiationSample {
        let id: xdr::Hash = [0; 32].into();
        let wasm: Vec<u8> = soroban_test_wasms::COMPLEX.clone().into();
        VmInstantiationSample { id, wasm }
    }

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> VmInstantiationSample {
        let id: xdr::Hash = [0; 32].into();
        let wasm = match rng.gen_range(0, 9) {
            0 => soroban_test_wasms::ADD_I32,
            1 => soroban_test_wasms::COMPLEX,
            2 => soroban_test_wasms::CONTRACT_DATA,
            3 => soroban_test_wasms::CREATE_CONTRACT,
            4 => soroban_test_wasms::FANNKUCH,
            5 => soroban_test_wasms::FIB,
            6 => soroban_test_wasms::HOSTILE,
            7 => soroban_test_wasms::LINEAR_MEMORY,
            8 => soroban_test_wasms::VEC,
            _ => unreachable!(),
        };
        let wasm: Vec<u8> = wasm.clone().into();
        VmInstantiationSample { id, wasm }
    }
}
