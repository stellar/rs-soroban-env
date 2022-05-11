/// Val is either RawVal or TaggedVal<T:TagType>, both of which
/// can be AsRef/AsMut'ed to RawVal.
///
/// This is a sort of hack to work around not having inheritance
/// in Rust.
use crate::{RawVal, TagType, TaggedVal};

pub trait Val: AsRef<RawVal> + AsMut<RawVal> + Clone {}

impl<T: TagType> Val for TaggedVal<T> {}
impl Val for RawVal {}
