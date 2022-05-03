use super::val::{Tag, ValType};
use super::Val;
use std::hash::Hash;

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct BitSet(Val);

impl ValType for BitSet {
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::BitSet)
    }

    unsafe fn unchecked_from_val(v: Val) -> Self {
        BitSet(v)
    }
}

impl From<BitSet> for Val {
    #[inline(always)]
    fn from(b: BitSet) -> Self {
        b.0
    }
}

impl Hash for BitSet {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.get_payload().hash(state);
    }
}

impl PartialEq for BitSet {
    fn eq(&self, other: &Self) -> bool {
        self.0.get_payload() == other.0.get_payload()
    }
}

impl Eq for BitSet {}

impl PartialOrd for BitSet {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BitSet {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.get_body().cmp(&other.0.get_body())
    }
}

impl BitSet {
    pub const fn try_from_u64(u: u64) -> Result<BitSet, ()> {
        if u & 0x0fff_ffff_ffff_ffff == u {
            Ok(BitSet(unsafe { Val::from_body_and_tag(u, Tag::BitSet) }))
        } else {
            Err(())
        }
    }
    pub const fn to_u64(&self) -> u64 {
        self.0.get_body()
    }
}
