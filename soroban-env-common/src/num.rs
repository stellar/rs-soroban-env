use core::cmp::Ordering;

use crate::{
    declare_tag_based_signed_small_and_object_wrappers,
    declare_tag_based_small_and_object_wrappers,
    declare_tag_based_unsigned_small_and_object_wrappers, declare_tag_based_wrapper,
    raw_val::TAG_BITS, Compare, ConversionError, Env, RawVal, Tag,
};
pub use ethnum::{AsI256, AsU256, I256, U256};
use stellar_xdr::{ScVal, Uint256};

declare_tag_based_wrapper!(U32Val);
declare_tag_based_wrapper!(I32Val);

impl RawVal {
    #[inline(always)]
    pub const fn from_u32(u: u32) -> U32Val {
        unsafe { U32Val(RawVal::from_major_minor_and_tag(u, 0, Tag::U32Val)) }
    }

    #[inline(always)]
    pub const fn from_i32(i: i32) -> I32Val {
        unsafe { I32Val(RawVal::from_major_minor_and_tag(i as u32, 0, Tag::I32Val)) }
    }

    #[inline(always)]
    pub const fn is_i32_zero(self) -> bool {
        self.shallow_eq(&Self::I32_ZERO.0)
    }

    #[inline(always)]
    pub const fn is_u32_zero(self) -> bool {
        self.shallow_eq(&Self::U32_ZERO.0)
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
        u128::from(value.0.get_body())
    }
}

impl From<I128Small> for i128 {
    fn from(value: I128Small) -> Self {
        i128::from(value.0.get_signed_body())
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
                RawVal::from_body_and_tag(value, Tag::U64Small)
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
                RawVal::from_body_and_tag(value as u64, Tag::I64Small)
            }))
        } else {
            Err(ConversionError)
        }
    }
}

impl TryFrom<u128> for U128Small {
    type Error = ConversionError;
    fn try_from(value: u128) -> Result<Self, Self::Error> {
        if is_small_u128(value) {
            Ok(Self(unsafe {
                RawVal::from_body_and_tag(value as u64, Tag::U128Small)
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
                RawVal::from_body_and_tag((value as u128) as u64, Tag::I128Small)
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
                RawVal::from_body_and_tag(value.as_u64(), Tag::U256Small)
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
                RawVal::from_body_and_tag(value.as_i64() as u64, Tag::I256Small)
            }))
        } else {
            Err(ConversionError)
        }
    }
}

impl TryFrom<U256Small> for ScVal {
    type Error = ConversionError;
    fn try_from(value: U256Small) -> Result<Self, Self::Error> {
        let val = U256::new(u128::from(value.as_raw().get_body()));
        Ok(ScVal::U256(Uint256(val.to_be_bytes())))
    }
}

impl TryFrom<&U256Small> for ScVal {
    type Error = ConversionError;
    fn try_from(value: &U256Small) -> Result<Self, Self::Error> {
        (*value).try_into()
    }
}

impl TryFrom<I256Small> for ScVal {
    type Error = ConversionError;
    fn try_from(value: I256Small) -> Result<Self, Self::Error> {
        let val = I256::new(i128::from(value.as_raw().get_signed_body()));
        Ok(ScVal::I256(Uint256(val.to_be_bytes())))
    }
}

impl TryFrom<&I256Small> for ScVal {
    type Error = ConversionError;
    fn try_from(value: &I256Small) -> Result<Self, Self::Error> {
        (*value).try_into()
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
    is_small_u64(word) && u == u128::from(word)
}

pub fn is_small_i128(i: i128) -> bool {
    let word = i as i64;
    is_small_i64(word) && i == i128::from(word)
}

pub fn is_small_u256(u: &U256) -> bool {
    let word = u.as_u64();
    is_small_u64(word) && *u == U256::from(word)
}

pub fn is_small_i256(i: &I256) -> bool {
    let word = i.as_i64();
    is_small_i64(word) && *i == I256::from(word)
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
