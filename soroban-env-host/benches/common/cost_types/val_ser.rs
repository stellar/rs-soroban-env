use crate::common::HostCostMeasurement;
use rand::{rngs::StdRng, RngCore};
use soroban_env_common::xdr::ScVec;
use soroban_env_host::{cost_runner::ValSerRun, xdr::ScVal, Host};

pub(crate) const MAX_DEPTH: u64 = 100;

pub(crate) struct ValSerMeasure;

// This measures the costs of converting an ScVal into bytes.
impl HostCostMeasurement for ValSerMeasure {
    type Runner = ValSerRun;
    const STEP_SIZE: u64 = 256;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> (ScVal, Vec<u8>) {
        let len = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        let mut buf = vec![0; len as usize];
        rng.fill_bytes(buf.as_mut_slice());
        let v = ScVal::Bytes(buf.try_into().unwrap());
        (v, Vec::default())
    }

    // The worst case is a deeply nested structure looking like below:
    //
    //                 Vec
    //
    //             /     |     \
    //         Vec    Bytes(len) .. repeating X times ..
    //
    //    /     |     \
    //   Vec    Bytes(len) .. repeating X times ..
    //  ...
    //
    // The bytes `len` scales with the input. Because of the way read_xdr works,
    // it is invoked on the individual sub-structure (and budget charge occurs
    // whenever the invocation happens). So there will mostly be repeated calls
    // to charge(len), from each Bytes object, and len is our calibration target
    // input. However, there will be a call to charge(4) which is due to the
    // u32::write_xdr at each Vec level. This will interfere will our
    // calibration, which is targeted on `len`. Therefore we repeated the Bytes
    // a number of times (that number can't be too large either, or the result
    // will just look like bytes clone) on each level to minimize the
    // interference of u32.

    fn new_worst_case(_host: &Host, rng: &mut StdRng, input: u64) -> (ScVal, Vec<u8>) {
        let len = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        let mut buf = vec![0; len as usize];
        rng.fill_bytes(buf.as_mut_slice());
        let scv_bytes = ScVal::Bytes(buf.try_into().unwrap());
        let mut scv_vec = ScVal::Vec(Some(ScVec(vec![scv_bytes.clone(); 1].try_into().unwrap())));
        for _i in 1..MAX_DEPTH {
            let mut v = vec![scv_vec; 1];
            v.append(&mut vec![scv_bytes.clone(); 10]);
            scv_vec = ScVal::Vec(Some(v.try_into().unwrap()));
        }
        (scv_vec, Vec::default())
    }
}
