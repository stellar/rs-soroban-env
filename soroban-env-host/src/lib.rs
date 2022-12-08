//! This crate mainly exists to provide the Soroban [Host] type, which is the
//! _implementation_ of the [Env] interface between guest contract code and the
//! host it runs within.
//!
//! This crate also re-exports all of the content of the [soroban_env_common]
//! crate for use by host (or contract local-testing) code. Most of the type and
//! module definitions visible here are actually defined in the common crate.
//!
//! The `Host` can be configured with or without support for a [vm::Vm],
//! depending on the `"vm"` cargo feature. When enabled, the VM is currently a
//! thin wrapper around the [wasmi](https://github.com/paritytech/wasmi)
//! interpreter, though other VMs might be supported in the future.
//!
//! It may seem unusual to configure a contract host _without_ a VM, but this
//! configuration makes more sense when considering that Soroban supports a
//! "local testing" configuration where host and guest code are _both compiled
//! natively_ and linked together for a faster and richer debugging and testing
//! experience. When testing this way, developers may also wish to enable the
//! `"testutils"` feature, which enables an interface on [Host] for registering
//! other test contracts by ID.
//!
//! The [Host] type provides some facilities above and beyond just the [Env]
//! trait, including:
//!
//!   - The [budget] module which is responsible for measuring and limiting
//!     execution costs in terms of CPU and memory.
//!   - The [storage] module which is responsible for providing an interface
//!     between contracts and their durable storage.
//!

pub mod budget;
pub mod events;
mod host;
pub(crate) mod host_object;

mod native_contract;

#[cfg(feature = "vm")]
pub mod vm;
#[cfg(feature = "vm")]
pub use vm::Vm;
pub mod storage;
#[cfg(test)]
mod test;

pub mod cost_runner;

#[cfg(any(test, feature = "testutils"))]
pub use host::ContractFunctionSet;
pub use host::{
    metered_map::MeteredOrdMap, metered_vector::MeteredVector, Host, HostError, LedgerInfo,
};
pub use soroban_env_common::*;
