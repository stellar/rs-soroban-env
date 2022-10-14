use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, Rng};
use soroban_env_host::{budget::CostType, xdr, Host, Vm};

pub(crate) struct VmInstantiationRun {
    id: xdr::Hash,
    wasm: Vec<u8>,
}

impl HostCostMeasurement for VmInstantiationRun {
    const COST_TYPE: CostType = CostType::VmInstantiation;
    const RUN_ITERATIONS: u64 = 10;

    fn new_best_case(_host: &Host, _rng: &mut StdRng) -> Self {
        let id: xdr::Hash = [0; 32].into();
        let wasm: Vec<u8> = soroban_test_wasms::ADD_I32.clone().into();
        Self { id, wasm }
    }

    fn new_worst_case(_host: &Host, _rng: &mut StdRng, _input: u64) -> Self {
        let id: xdr::Hash = [0; 32].into();
        let wasm: Vec<u8> = soroban_test_wasms::COMPLEX.clone().into();
        Self { id, wasm }
    }

    fn new_random_case(_host: &Host, rng: &mut StdRng, _input: u64) -> Self {
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
        Self { id, wasm }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.wasm.len() as u64
    }

    fn run(&mut self, _iter: u64, host: &Host) {
        Vm::new(host, self.id.clone(), &self.wasm[..]).unwrap();
    }
}
