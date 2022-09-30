mod base_types;
mod invoker;
mod token;

use crate::host::{Host, HostError};
use soroban_env_common::{RawVal, Symbol};

pub trait NativeContract {
    fn call(&self, func: &Symbol, host: &Host, args: &[RawVal]) -> Result<RawVal, HostError>;
}

pub use token::Token;
