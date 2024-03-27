#![allow(clippy::unit_arg)]
mod cost_types;
mod experimental;
mod runner;
mod util;

pub use cost_types::*;
pub use experimental::*;
pub use runner::CostRunner;

use crate::xdr::Name;
use core::fmt;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CostType {
    Contract(crate::xdr::ContractCostType),
    Experimental(experimental::ExperimentalCostType),
    Wasm(WasmInsnType),
}

impl Name for CostType {
    fn name(&self) -> &'static str {
        match self {
            CostType::Contract(ct) => ct.name(),
            CostType::Experimental(et) => et.name(),
            CostType::Wasm(wt) => wt.name(),
        }
    }
}

impl fmt::Display for CostType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
