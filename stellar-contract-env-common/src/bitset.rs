use crate::{decl_tagged_val_wrapper, ConversionError, Env, EnvVal, RawVal, Tag};
use core::cmp::Ordering;
use core::fmt::Debug;
use core::hash::{Hash, Hasher};

decl_tagged_val_wrapper!(BitSet);

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

impl BitSet {
    #[inline(always)]
    pub const fn try_from_u64(u: u64) -> Result<BitSet, BitSetError> {
        if u & 0x0fff_ffff_ffff_ffff == u {
            Ok(unsafe { Self::from_body(u) })
        } else {
            Err(BitSetError::TooManyBits(u))
        }
    }

    #[inline(always)]
    pub const fn to_u64(&self) -> u64 {
        self.0.get_body()
    }
}
