use super::val::{Tag, ValType};
use super::Val;
use core::cmp::Ordering;
use core::hash::{Hash, Hasher};

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

impl AsRef<Val> for BitSet {
    #[inline(always)]
    fn as_ref(&self) -> &Val {
        &self.0
    }
}

impl AsMut<Val> for BitSet {
    #[inline(always)]
    fn as_mut(&mut self) -> &mut Val {
        &mut self.0
    }
}

impl Hash for BitSet {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.get_payload().hash(state);
    }
}

impl PartialEq for BitSet {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.0.get_payload() == other.0.get_payload()
    }
}

impl Eq for BitSet {}

impl PartialOrd for BitSet {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BitSet {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.get_body().cmp(&other.0.get_body())
    }
}

impl BitSet {
    #[inline(always)]
    pub const fn try_from_u64(u: u64) -> Result<BitSet, ()> {
        if u & 0x0fff_ffff_ffff_ffff == u {
            Ok(BitSet(unsafe { Val::from_body_and_tag(u, Tag::BitSet) }))
        } else {
            Err(())
        }
    }
    #[inline(always)]
    pub const fn to_u64(&self) -> u64 {
        self.0.get_body()
    }
}
