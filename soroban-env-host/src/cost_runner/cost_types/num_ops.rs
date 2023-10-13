use std::hint::black_box;

use crate::{
    cost_runner::{CostRunner, CostType},
    xdr::ContractCostType::{Int256AddSub, Int256Div, Int256Mul, Int256Pow, Int256Shift},
    Env, I256Val, U32Val,
};

macro_rules! impl_int256_cost_runner {
    ($runner:ident, $method:ident, $cost:ident, $sample_type: ty) => {
        pub struct $runner;

        impl CostRunner for $runner {
            const COST_TYPE: CostType = CostType::Contract($cost);

            type SampleType = $sample_type;

            type RecycledType = Option<I256Val>;

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
                black_box(host.charge_budget($cost, None).unwrap());
                black_box(None)
            }
        }
    };
}
impl_int256_cost_runner!(Int256AddSubRun, i256_add, Int256AddSub, (I256Val, I256Val));
impl_int256_cost_runner!(Int256MulRun, i256_mul, Int256Mul, (I256Val, I256Val));
impl_int256_cost_runner!(Int256DivRun, i256_div, Int256Div, (I256Val, I256Val));
impl_int256_cost_runner!(Int256PowRun, i256_pow, Int256Pow, (I256Val, U32Val));
impl_int256_cost_runner!(Int256ShiftRun, i256_shl, Int256Shift, (I256Val, U32Val));
