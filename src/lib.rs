#![cfg_attr(not(feature = "host_context"), no_std)]

mod bitset;
mod host;
#[cfg(feature = "host_context")]
mod host_context;
mod object;
mod or_abort;
mod rt;
mod status;
mod symbol;
mod val;

use core::panic;

pub use bitset::BitSet;
pub use host::{Host, HostConvertable};
pub use object::{ObjType, Object};
pub use status::{Status, OK, UNKNOWN_ERROR};
pub use symbol::Symbol;
pub use val::{Val, ValType};

#[cfg(feature = "host_context")]
pub use host_context::{HostContext, HostObject};

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
