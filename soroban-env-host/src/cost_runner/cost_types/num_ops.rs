use std::hint::black_box;

use crate::{cost_runner::CostRunner, xdr::ContractCostType, Env, I256Object, U32Val};

macro_rules! impl_int256_cost_runner {
    ($runner:ident, $method:ident, $cost:ident, $sample_type: ty) => {
        pub struct $runner;

        impl CostRunner for $runner {
            const COST_TYPE: ContractCostType = ContractCostType::$cost;

            type SampleType = $sample_type;

            type RecycledType = Option<I256Object>;

            fn run_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                let res = black_box(host.$method(sample.0, sample.1).ok());
                res
            }

            fn run_baseline_iter(
                host: &crate::Host,
                _iter: u64,
                _sample: Self::SampleType,
            ) -> Self::RecycledType {
                black_box(host.charge_budget(Self::COST_TYPE, None).unwrap());
                black_box(None)
            }
        }
    };
}
impl_int256_cost_runner!(
    Int256AddSubRun,
    i256_add,
    Int256AddSub,
    (I256Object, I256Object)
);
impl_int256_cost_runner!(Int256MulRun, i256_mul, Int256Mul, (I256Object, I256Object));
impl_int256_cost_runner!(Int256DivRun, i256_div, Int256Div, (I256Object, I256Object));
impl_int256_cost_runner!(Int256PowRun, i256_pow, Int256Pow, (I256Object, U32Val));
impl_int256_cost_runner!(Int256ShiftRun, i256_shl, Int256Shift, (I256Object, U32Val));
