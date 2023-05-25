use crate::{
    declare_tag_based_object_wrapper, declare_tag_based_wrapper, impl_rawval_wrapper_base,
    impl_tryfroms_and_tryfromvals_delegating_to_rawvalconvertible, Compare, I32Val, SymbolSmall,
    SymbolStr, U32Val,
};
use stellar_xdr::{ScError, ScValType};

use super::{Env, Error, TryFromVal};
use core::{cmp::Ordering, convert::Infallible, fmt::Debug};

extern crate static_assertions as sa;

// A Soroban Val is a 64-bit word with the low 8 bits assigned to a `tag`
// indicating its type and the high 56 bits reserved for its `body`

#[allow(dead_code)]
const WORD_BITS: usize = 64;
pub(crate) const TAG_BITS: usize = 8;
const TAG_MASK: u64 = (1u64 << TAG_BITS) - 1;
sa::const_assert!(TAG_MASK == 0xff);

#[allow(dead_code)]
pub(crate) const BODY_BITS: usize = WORD_BITS - TAG_BITS;
sa::const_assert!(BODY_BITS == 56);

// The body is sometimes further subdivided into two fields:
// a 32-bit `major` part and a 24-bit `minor` part.

#[allow(dead_code)]
const MAJOR_BITS: usize = 32;
const MINOR_BITS: usize = 24;
#[allow(dead_code)]
const MAJOR_MASK: u64 = (1u64 << MAJOR_BITS) - 1;
const MINOR_MASK: u64 = (1u64 << MINOR_BITS) - 1;
sa::const_assert!(MAJOR_MASK == 0xffff_ffff);
sa::const_assert!(MINOR_MASK == 0x00ff_ffff);
sa::const_assert!(MAJOR_BITS + MINOR_BITS == BODY_BITS);

/// Code values for the 8 `tag` bits in the bit-packed representation
/// of [RawVal]. These don't coincide with tag numbers in the SCVal XDR
/// but cover all those cases as well as some optimized refinements for
/// special cases (boolean true and false, small-value forms).
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(test, derive(int_enum::IntEnum))]
pub enum Tag {
    /// Tag for a [RawVal] that encodes [bool] `false`. The bool type is refined to
    /// two single-value subtypes in order for each tag number to coincides with
    /// the WASM encoding of a boolean.
    False = 0,

    /// Tag for a [RawVal] that encodes [bool] `true`.
    True = 1,

    /// Tag for a [RawVal] that is empty/absent (eg. void, null, nil, undefined, None)
    Void = 2,

    /// Tag for a [RawVal] that is contains an error code.
    Error = 3,

    /// Tag for a [RawVal] that contains a [u32] number.
    U32Val = 4,

    /// Tag for a [RawVal] that contains an [i32] number.
    I32Val = 5,

    /// Tag for a [RawVal] that contains a [u64] small enough to fit in 56 bits.
    U64Small = 6,

    /// Tag for a [RawVal] that contains an [i64] small enough to fit in 56 bits.
    I64Small = 7,

    /// Tag for a [RawVal] that contains a [u64] timepoint small enough to fit
    /// in 56 bits.
    TimepointSmall = 8,

    /// Tag for a [RawVal] that contains a [u64] duration small enough to fit in
    /// 56 bits.
    DurationSmall = 9,

    /// Tag for a [RawVal] that contains a [u128] small enough to fit in 56 bits.
    U128Small = 10,

    /// Tag for a [RawVal] that contains a [i128] small enough to fit in 56 bits.
    I128Small = 11,

    /// Tag for a [RawVal] that contains a [u256] small enough to fit in 56 bits.
    U256Small = 12,

    /// Tag for a [RawVal] that contains a [i256] small enough to fit in 56 bits.
    I256Small = 13,

    /// Tag for a [RawVal] that contains up to 9 character symbols.
    SymbolSmall = 14,

    /// Tag for a [RawVal] that corresponds to
    /// [stellar_xdr::ScVal::LedgerKeyContractExecutable]
    LedgerKeyContractExecutable = 15,

    /// Tag for a [RawVal] that corresponds to [stellar_xdr::ScVal::StorageType]
    StorageType = 16,

    /// Code delimiting the upper boundary of "small" types.
    SmallCodeUpperBound = 17,

    /// Tag reserved to indicate boundary between tags for "small" types with
    /// their payload packed into the remaining 56 bits of the [RawVal] and
    /// "object" types that are stored as host objects and referenced by
    /// [Object] handle.
    ObjectCodeLowerBound = 63,

    /// Tag for a [RawVal] that refers to a host-side [u64] number.
    U64Object = 64,

    /// Tag for a [RawVal] that refers to a host-side [i64] number.
    I64Object = 65,

    /// Tag for a [RawVal] that refers to a host-side [u64] number encoding a
    /// time-point (a count of seconds since the Unix epoch, Jan 1 1970 UTC).
    TimepointObject = 66,

    /// Tag for a [RawVal] that refers to a host-side [i64] number encoding a
    /// duration (a count of seconds).
    DurationObject = 67,

    /// Tag for a [RawVal] that refers to a host-side [u128] number.
    U128Object = 68,

    /// Tag for a [RawVal] that refers to a host-side [i128] number.
    I128Object = 69,

    /// Tag for a [RawVal] that refers to a host-side [u256] number.
    U256Object = 70,

    /// Tag for a [RawVal] that refers to a host-side [i256] number.
    I256Object = 71,

    BytesObject = 72,
    StringObject = 73,
    SymbolObject = 74,

    VecObject = 75,
    MapObject = 76,

    ContractExecutableObject = 77,
    AddressObject = 78,

    /// Tag for a [RawVal] that corresponds to
    /// [stellar_xdr::ScVal::LedgerKeyNonce] and refers to a host-side
    /// address object that specifies which address it's the nonce for.
    LedgerKeyNonceObject = 79,

    ObjectCodeUpperBound = 80,

    Bad = 0x7f,
}

impl Tag {
    pub const fn rawval_mask() -> i64 {
        TAG_MASK as i64
    }
    pub fn rawval_const(&self) -> i64 {
        *self as i64
    }
    pub const fn is_object(self) -> bool {
        let tu8 = self as u8;
        tu8 > (Tag::ObjectCodeLowerBound as u8) || tu8 < (Tag::ObjectCodeUpperBound as u8)
    }

    #[inline(always)]
    pub const fn from_u8(tag: u8) -> Tag {
        const A: u8 = Tag::SmallCodeUpperBound as u8;
        const B: u8 = Tag::ObjectCodeLowerBound as u8;
        const C: u8 = Tag::ObjectCodeUpperBound as u8;
        if !((tag < A) || (B < tag && tag < C)) {
            return Tag::Bad;
        }

        // Transmuting an integer to an enum is UB if outside the defined enum
        // value set, so we need to test above to be safe. Note that it's ok for
        // this to be a _little_ slow since it's not called in a lot
        // of small/tight paths, only when doing a switch-based comparison. Most
        // small paths call `has_tag` which tests a _known_ enum case against
        // the tag byte, and therefore doesn't need the range check.
        //
        // The `test_tag_from_u8` test should ensure this cast is correct.
        unsafe { ::core::mem::transmute(tag) }
    }

    /// Get the ScValType of the XDR type that corresponds to this tag.
    ///
    /// For use in the `Host::obj_cmp` comparison function so that comparison
    /// based on tags can be done identically to the `ScVal` type.
    ///
    /// Returns `None` for `Tag::Bad`, and for the three marker tags
    /// `SmallCodeUpperBound`, `ObjectCodeLowerBound`, `ObjectCodeUpperBound`.
    #[inline(always)]
    pub const fn get_scval_type(&self) -> Option<ScValType> {
        match *self {
            Tag::False => Some(ScValType::Bool),
            Tag::True => Some(ScValType::Bool),
            Tag::Void => Some(ScValType::Void),
            Tag::Error => Some(ScValType::Error),
            Tag::U32Val => Some(ScValType::U32),
            Tag::I32Val => Some(ScValType::I32),
            Tag::U64Small => Some(ScValType::U64),
            Tag::I64Small => Some(ScValType::I64),
            Tag::TimepointSmall => Some(ScValType::Timepoint),
            Tag::DurationSmall => Some(ScValType::Duration),
            Tag::U128Small => Some(ScValType::U128),
            Tag::I128Small => Some(ScValType::I128),
            Tag::U256Small => Some(ScValType::U256),
            Tag::I256Small => Some(ScValType::I256),
            Tag::SymbolSmall => Some(ScValType::Symbol),
            Tag::LedgerKeyContractExecutable => Some(ScValType::LedgerKeyContractExecutable),
            Tag::SmallCodeUpperBound => None,
            Tag::ObjectCodeLowerBound => None,
            Tag::U64Object => Some(ScValType::U64),
            Tag::I64Object => Some(ScValType::I64),
            Tag::TimepointObject => Some(ScValType::Timepoint),
            Tag::DurationObject => Some(ScValType::Duration),
            Tag::U128Object => Some(ScValType::U128),
            Tag::I128Object => Some(ScValType::I128),
            Tag::U256Object => Some(ScValType::U256),
            Tag::I256Object => Some(ScValType::I256),
            Tag::BytesObject => Some(ScValType::Bytes),
            Tag::StringObject => Some(ScValType::String),
            Tag::SymbolObject => Some(ScValType::Symbol),
            Tag::VecObject => Some(ScValType::Vec),
            Tag::MapObject => Some(ScValType::Map),
            Tag::ContractExecutableObject => Some(ScValType::ContractExecutable),
            Tag::AddressObject => Some(ScValType::Address),
            Tag::LedgerKeyNonceObject => Some(ScValType::LedgerKeyNonce),
            Tag::ObjectCodeUpperBound => None,
            Tag::StorageType => Some(ScValType::StorageType),
            Tag::Bad => None,
        }
    }
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct RawVal(u64);

impl Default for RawVal {
    fn default() -> Self {
        Self::from_void().into()
    }
}

// Impl AsRef/AsMut and TryFromVal<RawVal> so that clients can abstract over a
// wrapper-or-RawVal because all wrappers also impl these.
impl AsRef<RawVal> for RawVal {
    fn as_ref(&self) -> &RawVal {
        self
    }
}

impl AsMut<RawVal> for RawVal {
    fn as_mut(&mut self) -> &mut RawVal {
        self
    }
}

impl<E: Env> TryFromVal<E, RawVal> for RawVal {
    type Error = ConversionError;
    fn try_from_val(_env: &E, val: &RawVal) -> Result<Self, Self::Error> {
        Ok(*val)
    }
}

// Declare a few extra small-value wrapper types that don't live anywhere else.
declare_tag_based_wrapper!(Void);

impl From<()> for Void {
    fn from(_value: ()) -> Self {
        RawVal::VOID
    }
}

impl<E: Env> Compare<Void> for E {
    type Error = E::Error;
    fn compare(&self, _a: &Void, _b: &Void) -> Result<Ordering, Self::Error> {
        Ok(Ordering::Equal)
    }
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Bool(RawVal);
impl_rawval_wrapper_base!(Bool);

impl From<bool> for Bool {
    fn from(value: bool) -> Self {
        RawVal::from_bool(value)
    }
}
impl From<Bool> for bool {
    fn from(value: Bool) -> Self {
        value.0.is_true()
    }
}

impl RawValConvertible for Bool {
    fn is_val_type(v: RawVal) -> bool {
        v.is_true() || v.is_false()
    }

    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        Self(v)
    }
}

impl<E: Env> Compare<Bool> for E {
    type Error = E::Error;
    fn compare(&self, a: &Bool, b: &Bool) -> Result<Ordering, Self::Error> {
        let a: bool = (*a).into();
        let b: bool = (*b).into();
        Ok(a.cmp(&b))
    }
}

// Declare a few extra object wrapper types that don't live anywhere else.
declare_tag_based_object_wrapper!(VecObject);
declare_tag_based_object_wrapper!(MapObject);
declare_tag_based_object_wrapper!(ContractExecutableObject);
declare_tag_based_object_wrapper!(LedgerKeyNonceObject);
declare_tag_based_object_wrapper!(AddressObject);

// This is a 0-arg struct rather than an enum to ensure it completely compiles
// away, the same way `()` would, while remaining a separate type to allow
// conversion to a more-structured error code at a higher level.

/// Error type indicating a failure to convert some type to another; details
/// of the failed conversion will typically be written to the debug log.
#[derive(Debug, Eq, PartialEq)]
pub struct ConversionError;

impl From<Infallible> for ConversionError {
    fn from(_: Infallible) -> Self {
        unreachable!()
    }
}

impl From<stellar_xdr::Error> for ConversionError {
    fn from(_: stellar_xdr::Error) -> Self {
        ConversionError
    }
}

/// Trait abstracting over types that can be converted into [RawVal], similar to
/// [TryFrom] but with a different signature that enables generating slightly
/// more efficient conversion code. An implementation of `TryFrom<Val>` is also
/// provided for any type that implements `ValConvertible`.
pub trait RawValConvertible: Into<RawVal> + TryFrom<RawVal> {
    /// Returns `true` if `v` is in a union state compatible with `Self`.
    fn is_val_type(v: RawVal) -> bool;

    /// Converts the bits making up a `Val` into `Self` _without_ checking
    /// that the `Val` is tagged correctly, assuming that such a check has
    /// been performed elsewhere. It is the caller's responsibility to arrange
    /// that such checks have occurred before calling `unchecked_from_val`,
    /// which is why it is marked as `unsafe` (it does not represent a risk of
    /// memory-unsafety, merely "serious logic errors").
    unsafe fn unchecked_from_val(v: RawVal) -> Self;

    /// Attempt a conversion from `Val` to `Self`, returning `None` if the
    /// provided `Val` is not tagged correctly. By default this calls
    /// `Self::is_val_type` and `Self::unchecked_from_val`, but it can be
    /// customized on a type-by-type basis to avoid redundant tag tests and
    /// produce more efficient code, as it is done for `Static` values like
    /// `bool`.
    #[inline(always)]
    fn try_convert(v: RawVal) -> Option<Self> {
        if Self::is_val_type(v) {
            Some(unsafe { Self::unchecked_from_val(v) })
        } else {
            None
        }
    }
}

impl_tryfroms_and_tryfromvals_delegating_to_rawvalconvertible!(());
impl_tryfroms_and_tryfromvals_delegating_to_rawvalconvertible!(bool);
impl_tryfroms_and_tryfromvals_delegating_to_rawvalconvertible!(u32);
impl_tryfroms_and_tryfromvals_delegating_to_rawvalconvertible!(i32);
impl_tryfroms_and_tryfromvals_delegating_to_rawvalconvertible!(Error);

#[cfg(feature = "wasmi")]
impl wasmi::core::FromValue for RawVal {
    fn from_value(val: wasmi::core::Value) -> Option<Self> {
        if let wasmi::core::Value::I64(i) = val {
            Some(RawVal::from_payload(i as u64))
        } else {
            None
        }
    }
}

#[cfg(feature = "wasmi")]
impl From<RawVal> for wasmi::core::Value {
    fn from(v: RawVal) -> Self {
        wasmi::core::Value::I64(v.get_payload() as i64)
    }
}

// Manually implement all the residual pieces: RawValConvertibles
// and Froms.

impl RawValConvertible for () {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::Void)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(_v: RawVal) -> Self {}
}

impl RawValConvertible for bool {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::True) || v.has_tag(Tag::False)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        v.has_tag(Tag::True)
    }
    #[inline(always)]
    fn try_convert(v: RawVal) -> Option<Self> {
        if v.has_tag(Tag::True) {
            Some(true)
        } else if v.has_tag(Tag::False) {
            Some(false)
        } else {
            None
        }
    }
}

impl RawValConvertible for u32 {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::U32Val)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        v.get_major()
    }
}

impl RawValConvertible for i32 {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::I32Val)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        v.get_major() as i32
    }
}

impl From<bool> for RawVal {
    #[inline(always)]
    fn from(b: bool) -> Self {
        RawVal::from_bool(b).into()
    }
}

impl From<()> for RawVal {
    #[inline(always)]
    fn from(_: ()) -> Self {
        RawVal::from_void().into()
    }
}

impl From<&()> for RawVal {
    #[inline(always)]
    fn from(_: &()) -> Self {
        RawVal::from_void().into()
    }
}

impl From<u32> for RawVal {
    #[inline(always)]
    fn from(u: u32) -> Self {
        RawVal::from_u32(u).into()
    }
}

impl From<&u32> for RawVal {
    #[inline(always)]
    fn from(u: &u32) -> Self {
        RawVal::from_u32(*u).into()
    }
}

impl From<i32> for RawVal {
    #[inline(always)]
    fn from(i: i32) -> Self {
        RawVal::from_i32(i).into()
    }
}

impl From<&i32> for RawVal {
    #[inline(always)]
    fn from(i: &i32) -> Self {
        RawVal::from_i32(*i).into()
    }
}

impl From<ScError> for RawVal {
    fn from(er: ScError) -> Self {
        let e: Error = er.into();
        e.to_raw()
    }
}

impl From<&ScError> for RawVal {
    fn from(er: &ScError) -> Self {
        let e: Error = er.clone().into();
        e.to_raw()
    }
}

// Utility methods

impl RawVal {
    #[inline(always)]
    pub const fn get_payload(self) -> u64 {
        self.0
    }

    #[inline(always)]
    pub const fn from_payload(x: u64) -> Self {
        Self(x)
    }

    #[inline(always)]
    pub const fn shallow_eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }

    #[inline(always)]
    const fn get_tag_u8(self) -> u8 {
        (self.0 & TAG_MASK) as u8
    }

    #[inline(always)]
    pub const fn get_tag(self) -> Tag {
        let tag = self.get_tag_u8();
        Tag::from_u8(tag)
    }

    #[inline(always)]
    pub(crate) const fn get_body(self) -> u64 {
        self.0 >> TAG_BITS
    }

    #[inline(always)]
    pub(crate) const fn get_signed_body(self) -> i64 {
        (self.0 as i64) >> TAG_BITS
    }

    #[inline(always)]
    pub(crate) const fn has_tag(self, tag: Tag) -> bool {
        self.get_tag_u8() == tag as u8
    }

    #[inline(always)]
    pub fn is<T: RawValConvertible>(self) -> bool {
        T::is_val_type(self)
    }

    #[inline(always)]
    // This does no checking, so it can be used in const fns
    // below; it should not be made public.
    pub(crate) const unsafe fn from_body_and_tag(body: u64, tag: Tag) -> RawVal {
        RawVal((body << TAG_BITS) | (tag as u64))
    }

    #[inline(always)]
    // This also does not checking, is a crate-local helper.
    pub(crate) const unsafe fn from_major_minor_and_tag(
        major: u32,
        minor: u32,
        tag: Tag,
    ) -> RawVal {
        let major = major as u64;
        let minor = minor as u64;
        Self::from_body_and_tag((major << MINOR_BITS) | minor, tag)
    }

    #[inline(always)]
    pub(crate) const fn has_minor(self, minor: u32) -> bool {
        self.get_minor() == minor
    }

    #[inline(always)]
    pub(crate) const fn has_major(self, major: u32) -> bool {
        self.get_major() == major
    }

    #[inline(always)]
    pub(crate) const fn get_minor(self) -> u32 {
        (self.get_body() & MINOR_MASK) as u32
    }

    #[inline(always)]
    pub(crate) const fn get_major(self) -> u32 {
        (self.get_body() >> MINOR_BITS) as u32
    }

    #[inline(always)]
    pub const fn is_object(self) -> bool {
        let tag = self.get_tag_u8();
        tag > (Tag::ObjectCodeLowerBound as u8) && tag < (Tag::ObjectCodeUpperBound as u8)
    }

    #[inline(always)]
    pub const fn from_void() -> Void {
        unsafe { Void(RawVal::from_body_and_tag(0, Tag::Void)) }
    }

    #[inline(always)]
    pub const fn from_bool(b: bool) -> Bool {
        let tag = if b { Tag::True } else { Tag::False };
        unsafe { Bool(RawVal::from_body_and_tag(0, tag)) }
    }

    #[inline(always)]
    pub const fn is_void(self) -> bool {
        self.shallow_eq(&Self::VOID.0)
    }

    #[inline(always)]
    pub const fn is_true(self) -> bool {
        self.shallow_eq(&Self::TRUE.0)
    }

    #[inline(always)]
    pub const fn is_false(self) -> bool {
        self.shallow_eq(&Self::FALSE.0)
    }
}

impl RawVal {
    pub const I32_ZERO: I32Val = RawVal::from_i32(0);
    pub const I32_MIN: I32Val = RawVal::from_i32(i32::MIN);
    pub const I32_MAX: I32Val = RawVal::from_i32(i32::MAX);

    pub const U32_ZERO: U32Val = RawVal::from_u32(0);
    pub const U32_ONE: U32Val = RawVal::from_u32(1);
    pub const U32_MIN: U32Val = RawVal::from_u32(u32::MIN);
    pub const U32_MAX: U32Val = RawVal::from_u32(u32::MAX);

    pub const VOID: Void = RawVal::from_void();

    pub const TRUE: Bool = RawVal::from_bool(true);
    pub const FALSE: Bool = RawVal::from_bool(false);
}

impl Debug for RawVal {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        fn fmt_obj(name: &str, r: &RawVal, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            write!(f, "{}(obj#{})", name, r.get_major())
        }

        match self.get_tag() {
            Tag::U32Val => write!(f, "U32({})", self.get_major()),
            Tag::I32Val => write!(f, "I32({})", self.get_major() as i32),
            Tag::False => write!(f, "False"),
            Tag::True => write!(f, "True"),
            Tag::Void => write!(f, "Void"),
            Tag::Error => unsafe { <Error as RawValConvertible>::unchecked_from_val(*self) }.fmt(f),
            Tag::U64Small => write!(f, "U64({})", self.get_body()),
            Tag::I64Small => write!(f, "I64({})", self.get_signed_body()),
            Tag::TimepointSmall => write!(f, "Timepoint({})", self.get_body()),
            Tag::DurationSmall => write!(f, "Duration({})", self.get_body()),
            // These can't be bigger than u64/i64 so just cast to them.
            Tag::U128Small => write!(f, "U128({})", self.get_body()),
            Tag::I128Small => write!(f, "I128({})", { self.get_signed_body() }),
            // These can't be bigger than u64/i64 so just cast to them.
            Tag::U256Small => write!(f, "U256({})", self.get_body()),
            Tag::I256Small => write!(f, "I256({})", { self.get_signed_body() }),
            Tag::SymbolSmall => {
                let ss: SymbolStr =
                    unsafe { <SymbolSmall as RawValConvertible>::unchecked_from_val(*self) }.into();
                let s: &str = ss.as_ref();
                write!(f, "Symbol({s})")
            }
            Tag::LedgerKeyContractExecutable => write!(f, "LedgerKeyContractCode"),

            Tag::U64Object => fmt_obj("U64", self, f),
            Tag::I64Object => fmt_obj("I64", self, f),
            Tag::TimepointObject => fmt_obj("Timepoint", self, f),
            Tag::DurationObject => fmt_obj("Duration", self, f),
            Tag::U128Object => fmt_obj("U128", self, f),
            Tag::I128Object => fmt_obj("I128", self, f),
            Tag::U256Object => fmt_obj("U256", self, f),
            Tag::I256Object => fmt_obj("I256", self, f),
            Tag::BytesObject => fmt_obj("Bytes", self, f),
            Tag::StringObject => fmt_obj("String", self, f),
            Tag::SymbolObject => fmt_obj("Symbol", self, f),
            Tag::VecObject => fmt_obj("Vec", self, f),
            Tag::MapObject => fmt_obj("Map", self, f),
            Tag::ContractExecutableObject => fmt_obj("ContractCode", self, f),
            Tag::AddressObject => fmt_obj("Address", self, f),
            Tag::LedgerKeyNonceObject => fmt_obj("LedgerKeyAddressNonce", self, f),
            Tag::StorageType => write!(f, "StorageType({})", self.get_body()),

            Tag::Bad
            | Tag::SmallCodeUpperBound
            | Tag::ObjectCodeLowerBound
            | Tag::ObjectCodeUpperBound => {
                write!(
                    f,
                    "Bad(tag={:x},body={:x})",
                    self.get_tag_u8(),
                    self.get_body()
                )
            }
        }
    }
}

#[test]
#[cfg(feature = "std")]
fn test_debug() {
    use super::{Error, Object, SymbolSmall};
    use crate::{
        xdr::{ScError, ScErrorCode, ScErrorType},
        I64Small, U64Small,
    };
    assert_eq!(format!("{:?}", RawVal::from_void()), "Void");
    assert_eq!(format!("{:?}", RawVal::from_bool(true)), "True");
    assert_eq!(format!("{:?}", RawVal::from_bool(false)), "False");
    assert_eq!(format!("{:?}", RawVal::from_i32(10)), "I32(10)");
    assert_eq!(format!("{:?}", RawVal::from_i32(-10)), "I32(-10)");
    assert_eq!(format!("{:?}", RawVal::from_u32(10)), "U32(10)");
    assert_eq!(format!("{:?}", I64Small::try_from(10).unwrap()), "I64(10)");
    assert_eq!(
        format!("{:?}", I64Small::try_from(-10).unwrap()),
        "I64(-10)"
    );
    assert_eq!(format!("{:?}", U64Small::try_from(10).unwrap()), "U64(10)");
    assert_eq!(
        format!("{:?}", SymbolSmall::try_from_str("hello").unwrap()),
        "Symbol(hello)"
    );
    assert_eq!(
        format!("{:?}", Object::from_handle_and_tag(7, Tag::VecObject)),
        "Vec(obj#7)"
    );
    assert_eq!(
        format!(
            "{:?}",
            Error::from_scerror(ScError {
                type_: ScErrorType::Value,
                code: ScErrorCode::InvalidInput
            })
        ),
        "Error(Value, InvalidInput)"
    );
}

// `Tag::from_u8` is implemented by hand unsafely.
//
// This test ensures that all cases are correct by comparing to the
// macro-generated results of the int-enum crate, which is only enabled as a
// dev-dependency.
#[test]
fn test_tag_from_u8() {
    use int_enum::IntEnum;

    for i in 0_u8..=255 {
        let expected_tag = Tag::from_int(i);
        let actual_tag = Tag::from_u8(i);
        match expected_tag {
            Ok(
                Tag::SmallCodeUpperBound | Tag::ObjectCodeLowerBound | Tag::ObjectCodeUpperBound,
            ) => {
                assert_eq!(actual_tag, Tag::Bad);
            }
            Ok(expected_tag) => {
                assert_eq!(expected_tag, actual_tag);
                let i_again = actual_tag as u8;
                assert_eq!(i, i_again);
            }
            Err(_) => {
                assert_eq!(actual_tag, Tag::Bad);
            }
        }
    }
}
