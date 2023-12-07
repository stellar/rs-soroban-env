use soroban_env_host::{
    cost_runner::ValDeserRun, xdr::ScVal, xdr::WriteXdr, DEFAULT_XDR_RW_LIMITS,
};

use super::{ValSerMeasure, MAX_DEPTH};
use crate::common::HostCostMeasurement;

pub(crate) struct ValDeserMeasure;

impl HostCostMeasurement for ValDeserMeasure {
    type Runner = ValDeserRun;
    const STEP_SIZE: u64 = 128;

    fn new_random_case(
        host: &soroban_env_host::Host,
        rng: &mut rand::prelude::StdRng,
        input: u64,
    ) -> Vec<u8> {
        let scval = ValSerMeasure::new_random_case(host, rng, input);
        scval.0.to_xdr(DEFAULT_XDR_RW_LIMITS).unwrap()
    }

    // The worst cpu case is a deeply nested structure looking like this:
    //
    //                             Vec
    //                       /      |       \
    //                     Vec    U64(0)    U64(0)
    //
    //                /     |     \
    //             Vec    U64(0)    U64(0)
    //
    //        /     |     \
    //     Vec    U64(0)    U64(0)
    //  ...
    //
    // The total length is propotional to the input size, each input is an
    // additional U64 that needs to be read which is cheap by itself but the cpu
    // needs to navigate all the levels to get to it.
    //
    // There will be lots of intermediate vec! allocation, which results in
    // large mem_bytes in the measurements, but most of these are transient and
    // the final mem cost should be close to the bytes size of the input
    // structure. For this reason the memory cost parameter is not extracted
    // from this measurement but rather derived analytically, based on results
    // from `ValSer` measurement.
    fn new_worst_case(
        _host: &soroban_env_host::Host,
        _rng: &mut rand::prelude::StdRng,
        input: u64,
    ) -> Vec<u8> {
        let input = Self::INPUT_BASE_SIZE + input * Self::STEP_SIZE;
        let elem_per_level = (input + MAX_DEPTH) / MAX_DEPTH;
        let mut v = ScVal::U64(0);
        let mut rem = input;
        for _i in 0..MAX_DEPTH {
            if rem == 0 {
                break;
            }
            let mut inner = vec![v; 1];
            inner.append(&mut vec![ScVal::U64(0); elem_per_level as usize]);
            v = ScVal::Vec(Some(inner.try_into().unwrap()));
            rem -= elem_per_level;
        }
        v.to_xdr(DEFAULT_XDR_RW_LIMITS).unwrap()
    }
}
