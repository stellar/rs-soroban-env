use num_bigint::Sign;

use crate::{
    budget::CostType, cost_runner::CostRunner, host::metered_cmp::MeteredCmp, MeteredBigInt,
};

pub struct BigIntNewRun;
impl CostRunner for BigIntNewRun {
    const COST_TYPE: CostType = CostType::BigIntNew;
    type SampleType = u64;
    const RUN_ITERATIONS: u64 = 1000;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        MeteredBigInt::from_u64(host.budget_cloned(), sample).unwrap();
    }
}

pub struct BigIntAddSubRun;
impl CostRunner for BigIntAddSubRun {
    const COST_TYPE: CostType = CostType::BigIntAddSub;
    type SampleType = (MeteredBigInt, MeteredBigInt);

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.0.add(&sample.1).unwrap();
    }
}

pub struct BigIntMulRun;
impl CostRunner for BigIntMulRun {
    const COST_TYPE: CostType = CostType::BigIntMul;
    type SampleType = (MeteredBigInt, MeteredBigInt);

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.0.mul(&sample.1).unwrap();
    }
}

pub struct BigIntDivRemRun;
impl CostRunner for BigIntDivRemRun {
    const COST_TYPE: CostType = CostType::BigIntDivRem;
    type SampleType = (MeteredBigInt, MeteredBigInt);

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.0.div(&sample.1).unwrap();
    }
}

pub struct BigIntBitwiseOpRun;
impl CostRunner for BigIntBitwiseOpRun {
    const COST_TYPE: CostType = CostType::BigIntBitwiseOp;
    type SampleType = (MeteredBigInt, MeteredBigInt);

    fn run_iter(_host: &crate::Host, iter: u64, sample: Self::SampleType) {
        match iter % 3 {
            0 => sample.0.bitand(&sample.1).unwrap(),
            1 => sample.0.bitor(&sample.1).unwrap(),
            2 => sample.0.bitxor(&sample.1).unwrap(),
            _ => panic!(),
        };
    }
}

pub struct BigIntShiftRun;
impl CostRunner for BigIntShiftRun {
    const COST_TYPE: CostType = CostType::BigIntShift;
    type SampleType = (MeteredBigInt, MeteredBigInt);

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.0.shl(&sample.1).unwrap();
    }
}

pub struct BigIntCmpRun;
impl CostRunner for BigIntCmpRun {
    const COST_TYPE: CostType = CostType::BigIntCmp;
    type SampleType = (MeteredBigInt, MeteredBigInt);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample
            .0
            .metered_cmp(&sample.1, &host.budget_cloned())
            .unwrap();
    }
}

pub struct BigIntGcdLcmRun;
impl CostRunner for BigIntGcdLcmRun {
    const COST_TYPE: CostType = CostType::BigIntGcdLcm;
    type SampleType = (MeteredBigInt, MeteredBigInt);

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.0.lcm(&sample.1).unwrap();
    }
}

pub struct BigIntPowRun;
impl CostRunner for BigIntPowRun {
    const RUN_ITERATIONS: u64 = 1;
    const COST_TYPE: CostType = CostType::BigIntPow;
    type SampleType = (MeteredBigInt, MeteredBigInt);

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.0.pow(&sample.1).unwrap();
    }
}

pub struct BigIntPowModRun;
impl CostRunner for BigIntPowModRun {
    const RUN_ITERATIONS: u64 = 1;
    const COST_TYPE: CostType = CostType::BigIntPowMod;
    type SampleType = (MeteredBigInt, MeteredBigInt, MeteredBigInt);

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.0.modpow(&sample.1, &sample.2).unwrap();
    }
}

pub struct BigIntSqrtRun;
impl CostRunner for BigIntSqrtRun {
    const COST_TYPE: CostType = CostType::BigIntSqrt;
    type SampleType = MeteredBigInt;

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.sqrt().unwrap();
    }
}

pub struct BigIntFromBytesRun;
impl CostRunner for BigIntFromBytesRun {
    const COST_TYPE: CostType = CostType::BigIntFromBytes;
    type SampleType = Vec<u8>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        MeteredBigInt::from_bytes_be(host.budget_cloned(), Sign::Plus, &sample.as_slice()).unwrap();
    }
}

pub struct BigIntToBytesRun;
impl CostRunner for BigIntToBytesRun {
    const COST_TYPE: CostType = CostType::BigIntToBytes;
    type SampleType = MeteredBigInt;

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.to_bytes_be().unwrap();
    }
}

pub struct BigIntToRadixRun;
impl CostRunner for BigIntToRadixRun {
    const COST_TYPE: CostType = CostType::BigIntToRadix;
    const RUN_ITERATIONS: u64 = 1;
    type SampleType = (MeteredBigInt, u32);

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.0.to_radix_be(sample.1).unwrap();
    }
}
