mod base_types;
mod token;

use crate::host::Host;
use soroban_env_common::{RawVal, Symbol};

pub trait NativeContract {
    fn call(&self, func: &Symbol, host: &Host, args: &[RawVal]) -> Result<RawVal, ()>;
}

pub use token::Token;
