pub(crate) mod base_types;
mod invoker;
pub(crate) mod token;

use crate::host::{Host, HostError};
use soroban_env_common::{RawVal, Symbol};

pub trait NativeContract {
    fn call(&self, func: &Symbol, host: &Host, args: &[RawVal]) -> Result<RawVal, HostError>;
}

pub use token::Token;

#[cfg(test)]
pub(crate) mod testutils;
