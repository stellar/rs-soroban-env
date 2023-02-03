pub(crate) mod base_types;
pub(crate) mod contract_error;
pub(crate) mod token;

use crate::host::{Host, HostError};
use soroban_env_common::{RawVal, Symbol};

pub trait NativeContract {
    fn call(&self, func: &Symbol, host: &Host, args: &[RawVal]) -> Result<RawVal, HostError>;
}

pub use token::Token;

pub(crate) mod account_contract;

#[cfg(test)]
pub(crate) mod testutils;
