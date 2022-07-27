/// Val is either RawVal or one of the wrappers, all of which
/// can be AsRef/AsMut'ed to RawVal.
///
/// This is a sort of hack to work around not having inheritance
/// in Rust.
use crate::{BitSet, Object, RawVal, Static, Status, Symbol};

pub trait Val: AsRef<RawVal> + AsMut<RawVal> + Into<RawVal> + Clone {}

impl Val for RawVal {}
impl Val for Object {}
impl Val for Status {}
impl Val for Static {}
impl Val for Symbol {}
impl Val for BitSet {}
