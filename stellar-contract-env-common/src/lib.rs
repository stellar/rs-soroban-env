#![cfg_attr(not(feature = "std"), no_std)]

mod bitset;
mod env;
mod env_obj;
mod env_val;
mod or_abort;
mod raw_obj;
mod rt;
mod status;
mod symbol;
mod val;

pub use bitset::BitSet;
pub use env::Env;
pub use env_obj::EnvObj;
pub use env_val::{EnvVal, EnvValType};
pub use or_abort::OrAbort;
pub use raw_obj::RawObj;
pub use rt::trap;
pub use status::{Status, OK, UNKNOWN_ERROR};
pub use stellar_xdr as xdr;
pub use symbol::{Symbol, SymbolIter};
pub use val::{Tag, Val, ValType};

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
