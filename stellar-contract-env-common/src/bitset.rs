use crate::{RawVal, Tag, TagBitSet, TaggedVal};

use core::cmp::Ordering;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;

pub type BitSet = TaggedVal<TagBitSet>;

pub enum BitSetError {
    TooManyBits(u64),
}

impl Hash for BitSet {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().get_payload().hash(state);
    }
}

impl PartialEq for BitSet {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().get_payload() == other.as_ref().get_payload()
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
        self.as_ref().get_body().cmp(&other.as_ref().get_body())
    }
}

impl BitSet {
    #[inline(always)]
    pub const fn try_from_u64(u: u64) -> Result<BitSet, BitSetError> {
        if u & 0x0fff_ffff_ffff_ffff == u {
            Ok(Self(
                unsafe { RawVal::from_body_and_tag(u, Tag::BitSet) },
                PhantomData,
            ))
        } else {
            Err(BitSetError::TooManyBits(u))
        }
    }
    #[inline(always)]
    pub const fn to_u64(&self) -> u64 {
        self.0.get_body()
    }
}
