#[allow(unused)]
use crate::common::{util, HostCostMeasurement};
use rand::{rngs::StdRng, Rng, RngCore};
use soroban_env_host::{
    cost_runner::{VmMemReadRun, VmMemRunSample, VmMemWriteRun},
    xdr, Host, Vm,
};

// Measures the cost of reading a slice of VM linear memory into a buffer.
// Input is bytes to read. CPU and memory cost should both be linear.
// TODO: does this run grow memory if the input exceeds 64kB?
pub(crate) struct VmMemReadMeasure;
impl HostCostMeasurement for VmMemReadMeasure {
    type Runner = VmMemReadRun;

    fn new_random_case(host: &Host, _rng: &mut StdRng, input: u64) -> VmMemRunSample {
        let input = 1 + input * Self::STEP_SIZE;
        let buf = vec![0; input as usize];
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
        let input = 1 + input * Self::STEP_SIZE;
        let mut buf = vec![0; input as usize];
        rng.fill_bytes(buf.as_mut_slice());
        let id: xdr::Hash = [0; 32].into();
        let code = soroban_test_wasms::ADD_I32;
        let vm = Vm::new(&host, id, &code).unwrap();
        VmMemRunSample { vm, buf }
    }
}
