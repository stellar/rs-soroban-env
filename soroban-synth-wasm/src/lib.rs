//! This crate contains utility types for generating WASM modules for the
//! soroban smart contract system. It is built on top of the wasm-encoder crate,
//! adding two helpers:
//!
//!   - [`ModEmitter`] which manages types and imports and produces modules that
//!     contain the necessary metadata section to load into soroban's VM.
//!
//!   - [`FuncEmitter`] which provides helper methods for encoding calls to
//!     the soroban host functions, as defined in [`soroban_env_common::Env`],
//!     as well as working with the [`soroban_env_common::Val`] type.

mod func_emitter;
mod host_funcs;
mod mod_emitter;
#[cfg(test)]
mod test;

pub use func_emitter::{FuncEmitter, LocalRef, Operand};
pub use mod_emitter::{Arity, FuncRef, GlobalRef, ModEmitter, TypeRef};

// mod expr is gated on testutils because it uses Arbitrary and
// enables soroban-env-common/testutils to get its implementations
// of arbitrary, and we only want to enable that selectively (when
// the host we're linked to has also enabled common/testutils)
#[cfg(feature = "testutils")]
mod expr;
#[cfg(feature = "testutils")]
pub use expr::{Emit, Expr};
