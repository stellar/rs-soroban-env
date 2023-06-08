//! This crate mainly exists to provide the Soroban [Host] type, which is the
//! _implementation_ of the [Env] interface between guest contract code and the
//! host it runs within.
//!
//! This crate also re-exports all of the content of the [soroban_env_common]
//! crate for use by host (or contract local-testing) code. Most of the type and
//! module definitions visible here are actually defined in the common crate.
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
pub use events::diagnostic::DiagnosticLevel;
pub mod expiration_ledger_bumps;
mod host;
pub(crate) mod host_object;

mod native_contract;

pub mod auth;
pub mod vm;
pub use vm::Vm;
#[cfg(any(test, feature = "testutils"))]
pub mod cost_runner;
pub mod storage;
#[cfg(test)]
mod test;

#[cfg(any(test, feature = "testutils"))]
#[doc(hidden)]
pub use host::testutils::call_with_suppressed_panic_hook;
#[cfg(any(test, feature = "testutils"))]
pub use host::ContractFunctionSet;
pub use host::{
    metered_map::MeteredOrdMap, metered_vector::MeteredVector, Host, HostError, LedgerInfo, Seed,
    SEED_BYTES,
};
pub use soroban_env_common::*;

pub mod fees;
