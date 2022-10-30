use num_bigint::BigInt;
use num_integer::Integer;

use crate::{budget::CostType, cost_runner::CostRunner};
pub struct BigIntDivRemRun;

impl CostRunner for BigIntDivRemRun {
    const COST_TYPE: CostType = CostType::BigIntDivRem;
    type SampleType = (BigInt, BigInt);

    fn run_iter(_host: &crate::Host, _iter: u64, sample: &mut Self::SampleType) -> Option<u64> {
        sample.0.div_rem(&sample.1);
        Some(sample.0.bits())
    }
}
