#![cfg_attr(not(feature = "std"), no_std)]
//! The environment-common crate contains three families of types:
//!
//!   - The [RawVal] type, a 64-bit value type that is a union between several
//!     different types (numbers, booleans, symbols, object references), encoded
//!     via careful bit-packing.
//!   - Wrapper types ([Object], [Symbol], [Status], [Static], [BitSet]) that
//!     contain [RawVal] in a specific, known union state. These are also 64-bit
//!     values, but offer methods specific to the union state (eg. [Symbol] will
//!     interconvert with Rust strings).
//!   - The [Env] trait, which describes the _interface_ between guest and host
//!     code. In other words, `Env` describes a set of _host functions_ that
//!     must be implemented in a contract host, and can be called from a guest
//!     (or by the SDK). Methods on the [Env] trait can only pass 64-bit values,
//!     which are usually [RawVal] or one of the wrapper types.
//!
//! The crate additionally contains functions for interconversion between the
//! [RawVal] type and XDR types, and re-exports the XDR definitions from
//! [stellar_xdr] under the module [xdr].

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Version<'a> {
    pub pkg: &'a str,
    pub rev: &'a str,
    pub interface: u64,
    pub xdr: stellar_xdr::Version<'a>,
}

pub const VERSION: Version = Version {
    pkg: env!("CARGO_PKG_VERSION"),
    rev: env!("GIT_REVISION"),
    interface: meta::INTERFACE_VERSION,
    xdr: stellar_xdr::VERSION,
};

mod val_wrapper;

mod array;
mod bitset;
mod checked_env;
mod compare;
mod convert;
mod env;
mod env_val;
mod invoker;
pub mod meta;
mod object;
mod option;
mod raw_val;
mod result;
mod r#static;
mod status;
mod r#str;
mod symbol;
mod tuple;
mod unimplemented_env;
mod val;
mod vmcaller_checked_env;

// Re-export the XDR definitions
pub use stellar_xdr as xdr;

// RawVal and RawObj are the 64-bit transparent type.
pub use raw_val::{ConversionError, RawVal, RawValConvertible, Tag};
pub use val::Val;

// RawVal and EnvObj couple raw types to environments.
pub use checked_env::CheckedEnv;
pub use compare::Compare;
pub use convert::Convert;
pub use env::{call_macro_with_all_host_functions, Env, EnvBase};
pub use env_val::{FromVal, IntoVal, TryFromVal, TryIntoVal};
pub use unimplemented_env::UnimplementedEnv;
pub use vmcaller_checked_env::{VmCaller, VmCallerCheckedEnv};

// BitSet, Status and Symbol wrap RawVals.
// TODO: maybe these should wrap EnvVals?
pub use bitset::{BitSet, BitSetError};
pub use invoker::InvokerType;
pub use object::Object;
pub use r#static::Static;
pub use status::Status;
pub use symbol::{Symbol, SymbolError, SymbolIter, SymbolStr};

#[inline(always)]
// Awkward: this is a free function rather than a trait call because
// you can't have const trait calls. It calls panic! rather than
// rt::trap because trap can't be const because one of its versions
// is the wasm unreachable intruction. Ideally this would be a function
// that did panic! in a const context and rt::trap in a non-const
// context but it's not clear how to actually do that.
pub const fn require(b: bool) {
    if !b {
        panic!();
    }
}
