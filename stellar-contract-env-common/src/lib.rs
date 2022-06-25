#![cfg_attr(not(feature = "std"), no_std)]

mod bitset;
mod checked_env;
mod env;
mod env_val;
mod object;
mod raw_val;
mod status;
mod symbol;
mod tagged_val;
mod tuple;
mod unimplemented_env;
mod val;

// Re-export the XDR definitions
pub use stellar_xdr as xdr;

// RawVal and RawObj are the 64-bit transparent type.
pub use raw_val::{ConversionError, RawVal, RawValConvertible, Tag};

pub use tagged_val::{
    TagBitSet, TagI32, TagObject, TagStatic, TagStatus, TagSymbol, TagType, TagU32, TaggedVal,
};
pub use val::Val;

// RawVal and EnvObj couple raw types to environments.
pub use checked_env::CheckedEnv;
pub use env::{Env, EnvBase};
pub use env_val::{EnvVal, IntoEnvVal, IntoVal, TryFromVal};
pub use unimplemented_env::UnimplementedEnv;

// BitSet, Status and Symbol wrap RawVals.
// TODO: maybe these should wrap EnvVals?
pub use bitset::{BitSet, BitSetError};
pub use object::Object;
pub use status::{Status, OK, UNKNOWN_ERROR};
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
