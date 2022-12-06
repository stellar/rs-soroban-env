use crate::{impl_wrapper_common, ConversionError, RawVal, Tag};
use core::cmp::Ordering;
use core::fmt::Debug;
use core::hash::{Hash, Hasher};

/// Wrapper for a [RawVal] that is tagged with [Tag::BitSet], interpreting the
/// [RawVal]'s body as a small bitset (60-bits or fewer).
#[derive(Copy, Clone)]
pub struct BitSet(RawVal);

impl_wrapper_common!(BitSet);

/// Errors related to operations on the [BitSet] type.
#[derive(Debug)]
pub enum BitSetError {
    /// Returned when attempting to form a [BitSet] from a [u64] with more than
    /// 60 bits set.
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

impl BitSet {
    /// Attempt to construct a [BitSet] from a u64, succeeding only if
    /// the most significant 4 bits of the u64 are clear. In other words,
    /// this function accepts only "small" bitsets of 60 or fewer bits.
    #[inline(always)]
    pub const fn try_from_u64(u: u64) -> Result<BitSet, BitSetError> {
        if u & 0x0fff_ffff_ffff_ffff == u {
            Ok(unsafe { Self::from_body(u) })
        } else {
            Err(BitSetError::TooManyBits(u))
        }
    }

    #[inline(always)]
    // Returns the 60-bit "body" of the contained [RawVal] as a [u64].
    pub const fn to_u64(&self) -> u64 {
        self.0.get_body()
    }
}
