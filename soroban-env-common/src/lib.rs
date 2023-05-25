#![cfg_attr(not(feature = "std"), no_std)]
//! The environment-common crate contains three families of types:
//!
//!   - The [RawVal] type, a 64-bit value type that is a union between several
//!     different types (numbers, booleans, symbols, object references), encoded
//!     via careful bit-packing.
//!   - Wrapper types ([Object], [Symbol], [Error]) that
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

mod wrapper_macros;

mod arbitrary;
mod bytes;
mod compare;
mod convert;
mod env;
mod env_val;
mod error;
mod object;
mod option;
mod raw_val;
mod result;
mod storage_type;
mod string;
mod symbol;
mod tuple;
mod unimplemented_env;
mod vmcaller_env;

// We have some types that we don't re-export everything
// from because only specific users are likely to use them.
pub mod meta;
pub mod num;
pub use num::{
    DurationObject, I128Object, I256Object, I64Object, TimepointObject, U128Object, U256Object,
    U64Object,
};
pub use num::{
    DurationSmall, I128Small, I256Small, I64Small, TimepointSmall, U128Small, U256Small, U64Small,
};
pub use num::{
    DurationVal, I128Val, I256Val, I32Val, I64Val, TimepointVal, U128Val, U256Val, U32Val, U64Val,
};
pub use num::{I256, U256};

pub use storage_type::StorageType;

// Re-export the XDR definitions
pub use stellar_xdr as xdr;

// RawVal is the 64-bit transparent type.
pub use raw_val::{
    AddressObject, ContractExecutableObject, LedgerKeyNonceObject, MapObject, VecObject,
};
pub use raw_val::{Bool, Void};
pub use raw_val::{ConversionError, RawVal, RawValConvertible, Tag};

pub use compare::Compare;
pub use convert::Convert;
pub use env::{call_macro_with_all_host_functions, Env, EnvBase};
pub use env_val::{TryFromVal, TryIntoVal};
pub use unimplemented_env::UnimplementedEnv;
pub use vmcaller_env::{VmCaller, VmCallerEnv};

pub use bytes::BytesObject;
pub use error::Error;
pub use object::{Object, ScValObjRef, ScValObject};
pub use string::StringObject;
pub use symbol::{Symbol, SymbolError, SymbolObject, SymbolSmall, SymbolSmallIter, SymbolStr};

#[inline(always)]
// Awkward: this is a free function rather than a trait call because
// you can't have const trait calls. It calls panic! rather than
// rt::trap because trap can't be const because one of its versions
// is the wasm unreachable intruction. Ideally this would be a function
// that did panic! in a const context and rt::trap in a non-const
// context but it's not clear how to actually do that.
pub const fn require(b: bool) {
    assert!(b,);
}
