use crate::{ConversionError, RawVal, RawValConvertible, Tag, TagBitSet, TaggedVal};
use core::cmp::Ordering;
use core::fmt::Debug;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;

pub struct BitSet(TaggedVal<TagBitSet>);

#[derive(Debug)]
pub enum BitSetError {
    TooManyBits(u64),
}

impl From<BitSetError> for ConversionError {
    fn from(_: BitSetError) -> Self {
        ConversionError
    }
}

impl Hash for BitSet {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_raw().get_payload().hash(state);
    }
}

impl PartialEq for BitSet {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.as_raw().get_payload() == other.as_raw().get_payload()
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
        self.as_raw().get_body().cmp(&other.as_raw().get_body())
    }
}

impl Debug for BitSet {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "BitSet({:#b})", self.as_raw().get_body())
    }
}

impl AsRef<TaggedVal<TagBitSet>> for BitSet {
    fn as_ref(&self) -> &TaggedVal<TagBitSet> {
        self.as_tagged()
    }
}

impl AsRef<RawVal> for BitSet {
    fn as_ref(&self) -> &RawVal {
        self.as_raw()
    }
}

impl From<TaggedVal<TagBitSet>> for BitSet {
    fn from(tv: TaggedVal<TagBitSet>) -> Self {
        BitSet(tv)
    }
}

impl From<BitSet> for TaggedVal<TagBitSet> {
    fn from(b: BitSet) -> Self {
        b.to_tagged()
    }
}

impl From<BitSet> for RawVal {
    fn from(b: BitSet) -> Self {
        b.to_raw()
    }
}

impl RawValConvertible for BitSet {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        <TaggedVal<TagBitSet> as RawValConvertible>::is_val_type(v)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        BitSet(<TaggedVal<TagBitSet> as RawValConvertible>::unchecked_from_val(v))
    }
}

impl BitSet {
    pub const fn as_raw(&self) -> &RawVal {
        &self.0 .0
    }

    pub const fn to_raw(&self) -> RawVal {
        self.0 .0
    }

    pub const fn as_tagged(&self) -> &TaggedVal<TagBitSet> {
        &self.0
    }

    pub const fn to_tagged(&self) -> TaggedVal<TagBitSet> {
        self.0
    }

    #[inline(always)]
    pub const fn try_from_u64(u: u64) -> Result<BitSet, BitSetError> {
        if u & 0x0fff_ffff_ffff_ffff == u {
            Ok(Self(TaggedVal(
                unsafe { RawVal::from_body_and_tag(u, Tag::BitSet) },
                PhantomData,
            )))
        } else {
            Err(BitSetError::TooManyBits(u))
        }
    }

    #[inline(always)]
    pub const fn to_u64(&self) -> u64 {
        self.0 .0.get_body()
    }
}
