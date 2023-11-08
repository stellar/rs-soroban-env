use soroban_env_host::{
    cost_runner::ReadXdrByteArrayRun,
    xdr::{ScBytes, ScVal, WriteXdr},
    DEFAULT_XDR_RW_LIMITS,
};

use crate::common::{util::randvec, HostCostMeasurement};

pub(crate) struct ReadXdrByteArrayMeasure;

impl HostCostMeasurement for ReadXdrByteArrayMeasure {
    type Runner = ReadXdrByteArrayRun;

    fn new_random_case(
        _host: &soroban_env_host::Host,
        rng: &mut rand::prelude::StdRng,
        input: u64,
    ) -> Vec<u8> {
        let len = 1 + input * Self::STEP_SIZE;
        let vec = randvec(rng, len);
        let scval = ScVal::Bytes(ScBytes(vec.try_into().unwrap()));
        scval.to_xdr(DEFAULT_XDR_RW_LIMITS).unwrap()
    }
}
