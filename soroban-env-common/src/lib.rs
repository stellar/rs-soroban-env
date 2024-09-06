#![cfg_attr(not(feature = "std"), no_std)]
//! The environment-common crate contains three families of types:
//!
//!   - The [Val] type, a 64-bit value type that is a union between several
//!     different types (numbers, booleans, symbols, object references), encoded
//!     via careful bit-packing.
//!   - Wrapper types ([Object], [Symbol], [Error]) that contain [Val] in a
//!     specific, known union state. These are also 64-bit values, but offer
//!     methods specific to the union state (eg. [Symbol] will interconvert with
//!     Rust string types).
//!   - The [Env] trait, which describes the _interface_ between guest and host
//!     code. In other words, `Env` describes a set of _host functions_ that
//!     must be implemented in a contract host, and can be called from a guest
//!     (or by the SDK). Methods on the [Env] trait can only pass 64-bit values,
//!     which are usually [Val] or one of the wrapper types.
//!
//! The crate additionally contains functions for interconversion between the
//! [Val] type and XDR types, and re-exports the XDR definitions from
//! [stellar_xdr] under the module [xdr].

#[allow(unused_macros)]
#[cfg(all(not(target_family = "wasm"), feature = "tracy"))]
macro_rules! tracy_span {
    () => {
        tracy_client::span!()
    };
    ($name:expr) => {
        tracy_client::span!($name)
    };
}

#[allow(unused_macros)]
#[cfg(any(target_family = "wasm", not(feature = "tracy")))]
macro_rules! tracy_span {
    () => {
        ()
    };
    ($name:expr) => {
        ()
    };
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Version<'a> {
    pub pkg: &'a str,
    pub rev: &'a str,
    pub interface: xdr::ScEnvMetaEntryInterfaceVersion,
    pub xdr: stellar_xdr::Version<'a>,
}

pub const VERSION: Version = Version {
    pkg: env!("CARGO_PKG_VERSION"),
    rev: env!("GIT_REVISION"),
    interface: meta::INTERFACE_VERSION,
    xdr: stellar_xdr::VERSION,
};

mod wrapper_macros;

#[cfg(feature = "testutils")]
mod arbitrary;
mod bytes;
mod compare;
mod convert;
mod env;
mod error;

// mod hash contains "shallow" impls of Hash for `Val` wrappers that are risky
// to expose in general since they do not "look through" the env to their
// underlying objects, instead hashing the object references themselves. This is
// sufficiently dangerous that we guard it behind a specific feature that only
// an informed consumer should enable.
#[cfg(any(feature = "testutils", feature = "shallow-val-hash"))]
mod hash;

mod object;
mod option;
mod result;
mod storage_type;
mod string;
mod symbol;
mod tuple;
mod val;
mod vmcaller_env;

// We have some modules that we don't re-export everything
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

// Re-export the XDR definitions of a specific version -- curr or next -- of the xdr crate.
#[cfg(not(feature = "next"))]
pub use stellar_xdr::curr as xdr;
#[cfg(feature = "next")]
pub use stellar_xdr::next as xdr;

// Val is the 64-bit transparent type.
pub use val::{ConversionError, Tag, Val};

#[cfg(feature = "wasmi")]
pub use val::WasmiMarshal;
pub use val::{AddressObject, MapObject, VecObject};
pub use val::{Bool, Void};

pub use compare::Compare;
pub use convert::{Convert, TryFromVal, TryIntoVal};
pub use env::{call_macro_with_all_host_functions, CheckedEnvArg, Env, EnvBase};
pub use vmcaller_env::{VmCaller, VmCallerEnv};

pub use bytes::BytesObject;
pub use error::Error;
pub use object::{Object, ScValObjRef, ScValObject};
pub use string::StringObject;
pub use symbol::{Symbol, SymbolError, SymbolObject, SymbolSmall, SymbolSmallIter, SymbolStr};
