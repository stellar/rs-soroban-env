use super::raw_val::{RawValType, Tag};
use super::RawVal;
use core::cmp::Ordering;
use core::hash::{Hash, Hasher};

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct BitSet(RawVal);

pub enum BitSetError {
    TooManyBits(u64),
}

impl RawValType for BitSet {
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::BitSet)
    }

    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        BitSet(v)
    }
}

impl From<BitSet> for RawVal {
    #[inline(always)]
    fn from(b: BitSet) -> Self {
        b.0
    }
}

impl AsRef<RawVal> for BitSet {
    #[inline(always)]
    fn as_ref(&self) -> &RawVal {
        &self.0
    }
}

impl AsMut<RawVal> for BitSet {
    #[inline(always)]
    fn as_mut(&mut self) -> &mut RawVal {
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
    pub const fn try_from_u64(u: u64) -> Result<BitSet, BitSetError> {
        if u & 0x0fff_ffff_ffff_ffff == u {
            Ok(BitSet(unsafe { RawVal::from_body_and_tag(u, Tag::BitSet) }))
        } else {
            Err(BitSetError::TooManyBits(u))
        }
    }
    #[inline(always)]
    pub const fn to_u64(&self) -> u64 {
        self.0.get_body()
    }
}
