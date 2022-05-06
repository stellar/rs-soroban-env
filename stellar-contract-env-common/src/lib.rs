#![cfg_attr(not(feature = "std"), no_std)]

mod bitset;
mod env;
mod env_obj;
mod env_val;
mod has_env;
mod or_abort;
mod raw_obj;
mod raw_val;
mod rt;
mod status;
mod symbol;

// Re-export the XDR definitions
pub use stellar_xdr as xdr;

// Export some runtime interfaces
pub use or_abort::OrAbort;
pub use rt::trap;

// RawVal and RawObj are the 64-bit transparent type.
pub use raw_obj::RawObj;
pub use raw_val::{RawVal, RawValType, Tag};

// RawVal and EnvObj couple raw types to environments.
pub use env::Env;
pub use env_obj::EnvObj;
pub use env_val::{EnvVal, EnvValType};
pub use has_env::HasEnv;

// BitSet, Status and Symbol wrap RawVals.
// TODO: maybe these should wrap EnvVals?
pub use bitset::BitSet;
pub use status::{Status, OK, UNKNOWN_ERROR};
pub use symbol::{Symbol, SymbolIter};

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
