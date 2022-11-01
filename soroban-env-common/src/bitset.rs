use crate::{decl_tagged_val_wrapper_methods, Env, EnvVal, RawVal, Tag};
use core::fmt::Debug;
use core::hash::{Hash, Hasher};

///
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct BitSet(u128);

impl crate::abi::BufReadWrite for BitSet {
    type MemBuf = <u128 as crate::abi::BufReadWrite>::MemBuf;

    const ZERO_BUF: Self::MemBuf = <u128 as crate::abi::BufReadWrite>::ZERO_BUF;

    fn buf_write(self, b: &mut Self::MemBuf) {
        self.0.buf_write(b)
    }

    fn buf_read(b: &Self::MemBuf) -> Self {
        Self(<u128 as crate::abi::BufReadWrite>::buf_read(b))
    }

    fn buf_as_slice(b: &Self::MemBuf) -> &[u8] {
        b.as_slice()
    }

    fn buf_as_mut_slice(b: &mut Self::MemBuf) -> &mut [u8] {
        b.as_mut_slice()
    }
}

impl crate::abi::V128 for BitSet {
    fn v128_explode(self) -> (u64, u64) {
        self.0.v128_explode()
    }

    fn v128_implode(a: u64, b: u64) -> Self {
        Self(<u128 as crate::abi::V128>::v128_implode(a, b))
    }
}

decl_tagged_val_wrapper_methods!(BitSet);

impl From<BitSet> for RawVal {
    fn from(obj: BitSet) -> Self {
        unsafe { RawVal::from_payload_and_tag(obj.0, Tag::BitSet) }
    }
}

impl crate::RawValConvertible for BitSet {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::BitSet)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        BitSet(v.payload)
    }
}

impl Hash for BitSet {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Debug for BitSet {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "BitSet({:#b})", self.0)
    }
}

impl From<u128> for BitSet {
    fn from(bits: u128) -> Self {
        BitSet(bits)
    }
}

impl Into<u128> for BitSet {
    fn into(self) -> u128 {
        self.0
    }
}

impl BitSet {
    #[inline(always)]
    pub const fn to_u128(&self) -> u128 {
        self.0
    }
}
