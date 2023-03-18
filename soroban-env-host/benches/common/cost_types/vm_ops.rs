use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, Rng, RngCore};
use soroban_env_host::{
    cost_runner::{
        VmInstantiationRun, VmInstantiationSample, VmMemReadRun, VmMemRunSample, VmMemWriteRun,
    },
    xdr, Host, Vm,
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
        VmInstantiationSample { id: Some(id), wasm }
    }

    fn new_worst_case(_host: &Host, _rng: &mut StdRng, input: u64) -> VmInstantiationSample {
        let id: xdr::Hash = [0; 32].into();
        let wasm = match input % 10 {
            0 => soroban_test_wasms::ADD_I32,
            1 => soroban_test_wasms::CREATE_CONTRACT,
            2 => soroban_test_wasms::CONTRACT_DATA,
            3 => soroban_test_wasms::LINEAR_MEMORY,
            4 => soroban_test_wasms::VEC,
            5 => soroban_test_wasms::INVOKE_CONTRACT,
            6 => soroban_test_wasms::HOSTILE,
            7 => soroban_test_wasms::FIB,
            8 => soroban_test_wasms::FANNKUCH,
            9 => soroban_test_wasms::COMPLEX,
            _ => unreachable!(),
        }
        .clone()
        .into();
        VmInstantiationSample { id: Some(id), wasm }
    }

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> VmInstantiationSample {
        let id: xdr::Hash = [0; 32].into();
        let wasm = match rng.gen_range(0, 10) {
            0 => soroban_test_wasms::ADD_I32,
            1 => soroban_test_wasms::CREATE_CONTRACT,
            2 => soroban_test_wasms::CONTRACT_DATA,
            3 => soroban_test_wasms::LINEAR_MEMORY,
            4 => soroban_test_wasms::VEC,
            5 => soroban_test_wasms::INVOKE_CONTRACT,
            6 => soroban_test_wasms::HOSTILE,
            7 => soroban_test_wasms::FIB,
            8 => soroban_test_wasms::FANNKUCH,
            9 => soroban_test_wasms::COMPLEX,
            _ => unreachable!(),
        };
        let wasm: Vec<u8> = wasm.clone().into();
        VmInstantiationSample { id: Some(id), wasm }
    }
}

// Measures the cost of reading a slice of VM linear memory into a buffer.
// Input is bytes to read. CPU and memory cost should both be linear.
// TODO: does this run grow memory if the input exceeds 64kB?
pub(crate) struct VmMemReadMeasure;
impl HostCostMeasurement for VmMemReadMeasure {
    type Runner = VmMemReadRun;

    fn new_random_case(host: &Host, _rng: &mut StdRng, input: u64) -> VmMemRunSample {
        let input = (input * 1000) as usize;
        let buf = vec![0; input];
        let id: xdr::Hash = [0; 32].into();
        let code = soroban_test_wasms::ADD_I32;
        let vm = Vm::new(&host, id, &code).unwrap();
        VmMemRunSample { vm, buf }
    }
}

// Measures the cost of writing into a slice of VM linear memory.
// Input is bytes to write. CPU and memory cost should both be linear.
// TODO: does this run grow memory if the input exceeds 64kB?
pub(crate) struct VmMemWriteMeasure;
impl HostCostMeasurement for VmMemWriteMeasure {
    type Runner = VmMemWriteRun;

    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> VmMemRunSample {
        let input = (input * 1000) as usize;
        let mut buf = vec![0; input];
        rng.fill_bytes(buf.as_mut_slice());
        let id: xdr::Hash = [0; 32].into();
        let code = soroban_test_wasms::ADD_I32;
        let vm = Vm::new(&host, id, &code).unwrap();
        VmMemRunSample { vm, buf }
    }
}
