use num_bigint::BigInt;
use num_integer::Integer;

use crate::{budget::CostType, cost_runner::CostRunner};
pub struct BigIntDivRemRun;

impl CostRunner for BigIntDivRemRun {
    const COST_TYPE: CostType = CostType::BigIntDivRem;
    type SampleType = (BigInt, BigInt);

    fn run_iter(_host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.0.div_rem(&sample.1);
    }

    fn get_total_input(_host: &crate::Host, sample: &Self::SampleType) -> u64 {
        sample.0.bits() * Self::RUN_ITERATIONS
    }
}
