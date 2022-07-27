use crate::{BitSet, Object, RawVal, Static, Status, Symbol};

/// Val is either [RawVal] or one of the wrapper types, all of which
/// can be `AsRef`/`AsMut`'ed to `RawVal`.
///
/// This is a sort of workaround for Rust not having inheritance.
pub trait Val: AsRef<RawVal> + AsMut<RawVal> + Into<RawVal> + Clone {}

impl Val for RawVal {}
impl Val for Object {}
impl Val for Status {}
impl Val for Static {}
impl Val for Symbol {}
impl Val for BitSet {}
