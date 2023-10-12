use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_common::xdr::ScVec;
use soroban_env_host::{cost_runner::ValSerRun, xdr::ScVal, Host};

pub(crate) const MAX_DEPTH: u64 = 100;

pub(crate) struct ValSerMeasure;

// This measures the costs of converting an ScVal into bytes.
impl HostCostMeasurement for ValSerMeasure {
    type Runner = ValSerRun;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> (ScVal, Vec<u8>) {
        let len = 1 + input * Self::STEP_SIZE;
        let mut buf = vec![0; len as usize];
        rng.fill_bytes(buf.as_mut_slice());
        let v = ScVal::Bytes(buf.try_into().unwrap());
        (v, Vec::default())
    }

    // The worst case is a deeply nested structure, each level containing byte arrays of the
    // given length.
    fn new_worst_case(_host: &Host, rng: &mut StdRng, input: u64) -> (ScVal, Vec<u8>) {
        let len = 1 + input * Self::STEP_SIZE;
        let mut buf = vec![0; len as usize];
        rng.fill_bytes(buf.as_mut_slice());
        let scv_bytes = ScVal::Bytes(buf.try_into().unwrap());
        let mut scv_vec = ScVal::Vec(Some(ScVec(vec![scv_bytes.clone(); 1].try_into().unwrap())));
        for _i in 1..MAX_DEPTH {
            let mut v = vec![scv_vec; 1];
            // We duplicate the bytes 10 times at each level. Because the way `MeteredWrtie` works,
            // serializing the ScVec will end up calling u32::write_xdr, which has input size 4.
            // This can create noise with what we want to measure, which is input size len.
            // Duplicating the bytes array reduces the noise.
            v.append(&mut vec![scv_bytes.clone(); 10]);
            scv_vec = ScVal::Vec(Some(v.try_into().unwrap()));
        }
        (scv_vec, Vec::default())
    }
}
