use crate::{
    xdr::{ContractCostParamEntry, ScErrorCode, ScErrorType},
    HostError,
};
use core::fmt::{Debug, Display};

/// We provide a "cost model" object that evaluates a linear expression:
///
///    f(x) = a + b * Option<x>
///
/// Where a, b are "fixed" parameters at construction time (extracted from an
/// on-chain cost schedule, so technically not _totally_ fixed) and Option<x>
/// is some abstract input variable -- say, event counts or object sizes --
/// provided at runtime. If the input cannot be defined, i.e., the cost is
/// constant, input-independent, then pass in `None` as the input.
///
/// The same `CostModel` type, i.e. `CostType` (applied to different parameters
/// and variables) is used for calculating memory as well as CPU time.
///
/// The various `CostType`s are carefully choosen such that 1. their underlying
/// cost characteristics (both cpu and memory) at runtime can be described
/// sufficiently by a linear model and 2. they together encompass the vast
/// majority of available operations done by the `env` -- the host and the VM.
///
/// The parameters for a `CostModel` are calibrated empirically. See this crate's
/// benchmarks for more details.
pub trait HostCostModel {
    fn evaluate(&self, input: Option<u64>) -> Result<u64, HostError>;

    #[cfg(any(test, feature = "testutils", feature = "bench"))]
    fn reset(&mut self);
}

/// The number of bits to scale the linear term by. The linear coefficient has
/// been scaled by this factor during parameter fitting to retain more significant
/// digits. Thus to get the cost from the raw input, we need to scale the result
/// back by the same factor.
const COST_MODEL_LIN_TERM_SCALE_BITS: u32 = 7;

/// A helper type that wraps an u64 to signify the wrapped value have been scaled.
#[derive(Clone, Copy, Default, Debug)]
pub struct ScaledU64(pub(crate) u64);

impl ScaledU64 {
    pub const fn from_unscaled_u64(u: u64) -> Self {
        ScaledU64(u << COST_MODEL_LIN_TERM_SCALE_BITS)
    }

    pub const fn unscale(self) -> u64 {
        self.0 >> COST_MODEL_LIN_TERM_SCALE_BITS
    }

    pub const fn is_zero(&self) -> bool {
        self.0 == 0
    }

    pub const fn saturating_mul(&self, rhs: u64) -> Self {
        ScaledU64(self.0.saturating_mul(rhs))
    }

    pub const fn safe_div(&self, rhs: u64) -> Self {
        ScaledU64(match self.0.checked_div(rhs) {
            Some(v) => v,
            None => 0,
        })
    }
}

impl Display for ScaledU64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(feature = "bench")]
impl From<f64> for ScaledU64 {
    fn from(unscaled: f64) -> Self {
        let scaled = unscaled * ((1 << COST_MODEL_LIN_TERM_SCALE_BITS) as f64);
        // We err on the side of overestimation by applying `ceil` to the input.
        ScaledU64(scaled.ceil() as u64)
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct MeteredCostComponent {
    pub const_term: u64,
    pub lin_term: ScaledU64,
}

impl TryFrom<&ContractCostParamEntry> for MeteredCostComponent {
    type Error = HostError;

    fn try_from(entry: &ContractCostParamEntry) -> Result<Self, Self::Error> {
        if entry.const_term < 0 || entry.linear_term < 0 {
            return Err((ScErrorType::Context, ScErrorCode::InvalidInput).into());
        }
        Ok(MeteredCostComponent {
            const_term: entry.const_term as u64,
            lin_term: ScaledU64(entry.linear_term as u64),
        })
    }
}

impl TryFrom<ContractCostParamEntry> for MeteredCostComponent {
    type Error = HostError;

    fn try_from(entry: ContractCostParamEntry) -> Result<Self, Self::Error> {
        Self::try_from(&entry)
    }
}

impl HostCostModel for MeteredCostComponent {
    fn evaluate(&self, input: Option<u64>) -> Result<u64, HostError> {
        let const_term = self.const_term;
        match input {
            Some(input) => {
                let mut res = const_term;
                if !self.lin_term.is_zero() {
                    let lin_cost = self.lin_term.saturating_mul(input).unscale();
                    res = res.saturating_add(lin_cost)
                }
                Ok(res)
            }
            None => Ok(const_term),
        }
    }

    #[cfg(any(test, feature = "testutils", feature = "bench"))]
    fn reset(&mut self) {
        self.const_term = 0;
        self.lin_term = ScaledU64(0);
    }
}
