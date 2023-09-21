use core::cmp::Ordering;

use crate::xdr::{Int256Parts, ScVal, UInt256Parts};
use crate::{
    declare_tag_based_signed_small_and_object_wrappers,
    declare_tag_based_small_and_object_wrappers,
    declare_tag_based_unsigned_small_and_object_wrappers, declare_tag_based_wrapper, val::TAG_BITS,
    Compare, ConversionError, Env, Tag, Val,
};
pub use ethnum::{AsI256, AsU256, I256, U256};

declare_tag_based_wrapper!(U32Val);
declare_tag_based_wrapper!(I32Val);

impl Val {
    #[inline(always)]
    pub const fn from_u32(u: u32) -> U32Val {
        unsafe { U32Val(Val::from_major_minor_and_tag(u, 0, Tag::U32Val)) }
    }

    #[inline(always)]
    pub const fn from_i32(i: i32) -> I32Val {
        unsafe { I32Val(Val::from_major_minor_and_tag(i as u32, 0, Tag::I32Val)) }
    }
}

impl<E: Env> Compare<U32Val> for E {
    type Error = E::Error;
    fn compare(&self, a: &U32Val, b: &U32Val) -> Result<Ordering, Self::Error> {
        Ok(u32::from(*a).cmp(&u32::from(*b)))
    }
}

impl<E: Env> Compare<I32Val> for E {
    type Error = E::Error;
    fn compare(&self, a: &I32Val, b: &I32Val) -> Result<Ordering, Self::Error> {
        Ok(i32::from(*a).cmp(&i32::from(*b)))
    }
}

declare_tag_based_unsigned_small_and_object_wrappers!(U64Val, U64Small, U64Object);
declare_tag_based_signed_small_and_object_wrappers!(I64Val, I64Small, I64Object);
declare_tag_based_unsigned_small_and_object_wrappers!(
    TimepointVal,
    TimepointSmall,
    TimepointObject
);
declare_tag_based_unsigned_small_and_object_wrappers!(DurationVal, DurationSmall, DurationObject);

declare_tag_based_unsigned_small_and_object_wrappers!(U128Val, U128Small, U128Object);
declare_tag_based_signed_small_and_object_wrappers!(I128Val, I128Small, I128Object);
declare_tag_based_unsigned_small_and_object_wrappers!(U256Val, U256Small, U256Object);
declare_tag_based_signed_small_and_object_wrappers!(I256Val, I256Small, I256Object);

impl From<u32> for U32Val {
    fn from(value: u32) -> Self {
        U32Val(value.into())
    }
}

impl From<U32Val> for u32 {
    fn from(value: U32Val) -> Self {
        value.0.get_major()
    }
}

impl From<i32> for I32Val {
    fn from(value: i32) -> Self {
        I32Val(value.into())
    }
}

impl From<I32Val> for i32 {
    fn from(value: I32Val) -> Self {
        value.0.get_major() as i32
    }
}

impl From<U64Small> for u64 {
    fn from(value: U64Small) -> Self {
        value.0.get_body()
    }
}

impl From<I64Small> for i64 {
    fn from(value: I64Small) -> Self {
        value.0.get_signed_body()
    }
}

impl From<TimepointSmall> for u64 {
    fn from(value: TimepointSmall) -> Self {
        value.0.get_body()
    }
}

impl From<DurationSmall> for u64 {
    fn from(value: DurationSmall) -> Self {
        value.0.get_body()
    }
}

impl From<U128Small> for u128 {
    fn from(value: U128Small) -> Self {
        value.0.get_body() as u128
    }
}

impl From<I128Small> for i128 {
    fn from(value: I128Small) -> Self {
        value.0.get_signed_body() as i128
    }
}

impl From<U256Small> for U256 {
    fn from(value: U256Small) -> Self {
        U256::from(value.0.get_body())
    }
}

impl From<I256Small> for I256 {
    fn from(value: I256Small) -> Self {
        I256::from(value.0.get_signed_body())
    }
}

impl TryFrom<u64> for U64Small {
    type Error = ConversionError;
    fn try_from(value: u64) -> Result<Self, Self::Error> {
        if is_small_u64(value) {
            Ok(Self(unsafe {
                Val::from_body_and_tag(value, Tag::U64Small)
            }))
        } else {
            Err(ConversionError)
        }
    }
}

impl TryFrom<i64> for I64Small {
    type Error = ConversionError;
    fn try_from(value: i64) -> Result<Self, Self::Error> {
        if is_small_i64(value) {
            Ok(Self(unsafe {
                Val::from_body_and_tag(value as u64, Tag::I64Small)
            }))
        } else {
            Err(ConversionError)
        }
    }
}

impl TryFrom<u64> for TimepointSmall {
    type Error = ConversionError;
    fn try_from(value: u64) -> Result<Self, Self::Error> {
        if is_small_u64(value) {
            Ok(Self(unsafe {
                Val::from_body_and_tag(value, Tag::TimepointSmall)
            }))
        } else {
            Err(ConversionError)
        }
    }
}

impl TryFrom<u64> for DurationSmall {
    type Error = ConversionError;
    fn try_from(value: u64) -> Result<Self, Self::Error> {
        if is_small_u64(value) {
            Ok(Self(unsafe {
                Val::from_body_and_tag(value, Tag::DurationSmall)
            }))
        } else {
            Err(ConversionError)
        }
    }
}

impl TryFrom<TimepointSmall> for ScVal {
    type Error = ConversionError;
    fn try_from(value: TimepointSmall) -> Result<Self, ConversionError> {
        Ok(ScVal::Timepoint(value.as_val().get_body().into()))
    }
}

impl TryFrom<&TimepointSmall> for ScVal {
    type Error = ConversionError;
    fn try_from(value: &TimepointSmall) -> Result<Self, ConversionError> {
        (*value).try_into()
    }
}

impl TryFrom<DurationSmall> for ScVal {
    type Error = ConversionError;
    fn try_from(value: DurationSmall) -> Result<Self, ConversionError> {
        Ok(ScVal::Duration(value.as_val().get_body().into()))
    }
}

impl TryFrom<&DurationSmall> for ScVal {
    type Error = ConversionError;
    fn try_from(value: &DurationSmall) -> Result<Self, ConversionError> {
        (*value).try_into()
    }
}

impl TryFrom<u128> for U128Small {
    type Error = ConversionError;
    fn try_from(value: u128) -> Result<Self, Self::Error> {
        if is_small_u128(value) {
            Ok(Self(unsafe {
                Val::from_body_and_tag(value as u64, Tag::U128Small)
            }))
        } else {
            Err(ConversionError)
        }
    }
}

impl TryFrom<i128> for I128Small {
    type Error = ConversionError;
    fn try_from(value: i128) -> Result<Self, Self::Error> {
        if is_small_i128(value) {
            Ok(Self(unsafe {
                Val::from_body_and_tag((value as u128) as u64, Tag::I128Small)
            }))
        } else {
            Err(ConversionError)
        }
    }
}

impl TryFrom<U256> for U256Small {
    type Error = ConversionError;
    fn try_from(value: U256) -> Result<Self, Self::Error> {
        if is_small_u256(&value) {
            Ok(Self(unsafe {
                Val::from_body_and_tag(value.as_u64(), Tag::U256Small)
            }))
        } else {
            Err(ConversionError)
        }
    }
}

impl TryFrom<I256> for I256Small {
    type Error = ConversionError;
    fn try_from(value: I256) -> Result<Self, Self::Error> {
        if is_small_i256(&value) {
            Ok(Self(unsafe {
                Val::from_body_and_tag(value.as_i64() as u64, Tag::I256Small)
            }))
        } else {
            Err(ConversionError)
        }
    }
}

impl TryFrom<U256Small> for ScVal {
    type Error = ConversionError;
    fn try_from(value: U256Small) -> Result<Self, ConversionError> {
        let val = U256::new(value.as_val().get_body() as u128);
        let (hi_hi, hi_lo, lo_hi, lo_lo) = u256_into_pieces(val);
        Ok(ScVal::U256(UInt256Parts {
            hi_hi,
            hi_lo,
            lo_hi,
            lo_lo,
        }))
    }
}

impl TryFrom<&U256Small> for ScVal {
    type Error = ConversionError;
    fn try_from(value: &U256Small) -> Result<Self, ConversionError> {
        (*value).try_into()
    }
}

impl TryFrom<I256Small> for ScVal {
    type Error = ConversionError;
    fn try_from(value: I256Small) -> Result<Self, ConversionError> {
        let val = I256::new(value.as_val().get_signed_body() as i128);
        let (hi_hi, hi_lo, lo_hi, lo_lo) = i256_into_pieces(val);
        Ok(ScVal::I256(Int256Parts {
            hi_hi,
            hi_lo,
            lo_hi,
            lo_lo,
        }))
    }
}

impl TryFrom<&I256Small> for ScVal {
    type Error = ConversionError;
    fn try_from(value: &I256Small) -> Result<Self, ConversionError> {
        (*value).try_into()
    }
}

// Some explicit convenience "small" constructors that take 32-bit inputs in
// order to remain infallible and are also `const fn`.

impl I64Small {
    pub const fn from_i32(small: i32) -> Self {
        let extended = small as i64;
        unsafe { I64Small::from_body(extended as u64) }
    }
}

impl I64Val {
    pub const fn from_i32(small: i32) -> Self {
        Self::from_small(I64Small::from_i32(small))
    }
}

impl U64Small {
    pub const fn from_u32(small: u32) -> Self {
        let extended = small as u64;
        unsafe { U64Small::from_body(extended) }
    }
}

impl U64Val {
    pub const fn from_u32(small: u32) -> Self {
        Self::from_small(U64Small::from_u32(small))
    }
}

impl I128Small {
    pub const fn from_i32(small: i32) -> Self {
        let extended = small as i64;
        unsafe { I128Small::from_body(extended as u64) }
    }
}

impl I128Val {
    pub const fn from_i32(small: i32) -> Self {
        Self::from_small(I128Small::from_i32(small))
    }
}

impl U128Small {
    pub const fn from_u32(small: u32) -> Self {
        let extended = small as u64;
        unsafe { U128Small::from_body(extended) }
    }
}

impl U128Val {
    pub const fn from_u32(small: u32) -> Self {
        Self::from_small(U128Small::from_u32(small))
    }
}

impl I256Small {
    pub const fn from_i32(small: i32) -> Self {
        let extended = small as i64;
        unsafe { I256Small::from_body(extended as u64) }
    }
}

impl I256Val {
    pub const fn from_i32(small: i32) -> Self {
        Self::from_small(I256Small::from_i32(small))
    }
}

impl U256Small {
    pub const fn from_u32(small: u32) -> Self {
        let extended = small as u64;
        unsafe { U256Small::from_body(extended) }
    }
}

impl U256Val {
    pub const fn from_u32(small: u32) -> Self {
        Self::from_small(U256Small::from_u32(small))
    }
}

pub const fn is_small_u64(u: u64) -> bool {
    u == ((u << TAG_BITS) >> TAG_BITS)
}

pub const fn is_small_i64(i: i64) -> bool {
    i == ((i << TAG_BITS) >> TAG_BITS)
}

pub fn is_small_u128(u: u128) -> bool {
    let word = u as u64;
    is_small_u64(word) && u == (word as u128)
}

pub fn is_small_i128(i: i128) -> bool {
    let word = i as i64;
    is_small_i64(word) && i == (word as i128)
}

pub fn is_small_u256(u: &U256) -> bool {
    let word = u.as_u64();
    is_small_u64(word) && *u == U256::from(word)
}

pub fn is_small_i256(i: &I256) -> bool {
    let word = i.as_i64();
    is_small_i64(word) && *i == I256::from(word)
}

pub fn is_small_u256_parts(u: &UInt256Parts) -> bool {
    u.hi_hi == 0 && u.hi_lo == 0 && u.lo_hi == 0 && is_small_u64(u.lo_lo)
}

pub fn is_small_i256_parts(i: &Int256Parts) -> bool {
    let i = i256_from_pieces(i.hi_hi, i.hi_lo, i.lo_hi, i.lo_lo);
    is_small_i256(&i)
}

pub fn u256_from_pieces(hi_hi: u64, hi_lo: u64, lo_hi: u64, lo_lo: u64) -> U256 {
    let high = (u128::from(hi_hi)) << 64 | u128::from(hi_lo);
    let low = (u128::from(lo_hi)) << 64 | u128::from(lo_lo);
    U256::from_words(high, low)
}

pub fn u256_into_pieces(u: U256) -> (u64, u64, u64, u64) {
    let (high, low) = u.into_words();
    let (hi_hi, hi_lo) = ((high >> 64) as u64, high as u64);
    let (lo_hi, lo_lo) = ((low >> 64) as u64, low as u64);
    (hi_hi, hi_lo, lo_hi, lo_lo)
}

pub fn i256_from_pieces(hi_hi: i64, hi_lo: u64, lo_hi: u64, lo_lo: u64) -> I256 {
    let high = ((u128::from(hi_hi as u64) << 64) | u128::from(hi_lo)) as i128;
    let low = ((u128::from(lo_hi) << 64) | u128::from(lo_lo)) as i128;
    I256::from_words(high, low)
}

pub fn i256_into_pieces(i: I256) -> (i64, u64, u64, u64) {
    let (high, low) = i.into_words();
    let (hi_hi, hi_lo) = ((high >> 64) as i64, high as u64);
    let (lo_hi, lo_lo) = ((low >> 64) as u64, low as u64);
    (hi_hi, hi_lo, lo_hi, lo_lo)
}

pub const MIN_SMALL_U64: u64 = 0;
pub const MAX_SMALL_U64: u64 = 0x00ff_ffff_ffff_ffff_u64;

pub const MIN_SMALL_I64: i64 = 0xff80_0000_0000_0000_u64 as i64;
pub const MAX_SMALL_I64: i64 = 0x007f_ffff_ffff_ffff_u64 as i64;

static_assertions::const_assert!(MIN_SMALL_I64 == -36_028_797_018_963_968_i64);
static_assertions::const_assert!(MAX_SMALL_I64 == 36_028_797_018_963_967_i64);

pub const MIN_SMALL_U128: u128 = MIN_SMALL_U64 as u128;
pub const MAX_SMALL_U128: u128 = MAX_SMALL_U64 as u128;

pub const MIN_SMALL_I128: i128 = MIN_SMALL_I64 as i128;
pub const MAX_SMALL_I128: i128 = MAX_SMALL_I64 as i128;

pub const MIN_SMALL_U256: U256 = U256::new(MIN_SMALL_U128);
pub const MAX_SMALL_U256: U256 = U256::new(MAX_SMALL_U128);

pub const MIN_SMALL_I256: I256 = I256::new(MIN_SMALL_I128);
pub const MAX_SMALL_I256: I256 = I256::new(MAX_SMALL_I128);

#[test]
fn test_small_ints() {
    assert!(!is_small_i64(MIN_SMALL_I64 - 1));
    assert!(is_small_i64(MIN_SMALL_I64));
    assert!(is_small_i64(MAX_SMALL_I64));
    assert!(!is_small_i64(MAX_SMALL_I64 + 1));

    assert!(is_small_u64(MIN_SMALL_U64));
    assert!(is_small_u64(MAX_SMALL_U64));
    assert!(!is_small_u64(MAX_SMALL_U64 + 1));

    assert!(!is_small_i128(MIN_SMALL_I128 - 1));
    assert!(is_small_i128(MIN_SMALL_I128));
    assert!(is_small_i128(MAX_SMALL_I128));
    assert!(!is_small_i128(MAX_SMALL_I128 + 1));

    assert!(is_small_u128(MIN_SMALL_U128));
    assert!(is_small_u128(MAX_SMALL_U128));
    assert!(!is_small_u128(MAX_SMALL_U128 + 1));

    assert!(!is_small_i256(&(MIN_SMALL_I256 - 1)));
    assert!(is_small_i256(&(MIN_SMALL_I256)));
    assert!(is_small_i256(&(MAX_SMALL_I256)));
    assert!(!is_small_i256(&(MAX_SMALL_I256 + 1)));

    assert!(is_small_u256(&(MIN_SMALL_U256)));
    assert!(is_small_u256(&(MAX_SMALL_U256)));
    assert!(!is_small_u256(&(MAX_SMALL_U256 + 1)));

    assert!(is_small_i64(-1_i64));
    assert!(is_small_i64(-12345_i64));
    assert!(is_small_i64(1_i64));
    assert!(is_small_i64(12345_i64));

    assert!(is_small_i128(-1_i128));
    assert!(is_small_i128(-12345_i128));
    assert!(is_small_i128(1_i128));
    assert!(is_small_i128(12345_i128));

    assert!(is_small_i256(&I256::new(-1_i128)));
    assert!(is_small_i256(&I256::new(-12345_i128)));
    assert!(is_small_i256(&I256::new(1_i128)));
    assert!(is_small_i256(&I256::new(12345_i128)));
}
