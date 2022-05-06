mod host;
pub(crate) mod host_object;
pub(crate) mod weak_host;

#[cfg(feature = "vm")]
pub mod vm;

#[cfg(test)]
mod test;

pub use host::{Error, Host};
pub use stellar_contract_env_common::*;
