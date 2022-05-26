mod host;
pub(crate) mod host_object;
pub(crate) mod weak_host;

#[cfg(feature = "vm")]
pub mod vm;
#[cfg(feature = "vm")]
pub use vm::Vm;
pub mod storage;
#[cfg(test)]
mod test;

pub use host::{Host, HostError};
pub use stellar_contract_env_common::*;

// TODO: this should become xdr::ContractID when that type arrives.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ContractID(i64);
