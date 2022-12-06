use stellar_xdr::{ScStatic, ScStatus, ScStatusType};

use super::{
    BitSet, Env, FromVal, IntoVal, Object, Static, Status, Symbol, TryFromVal, TryIntoVal,
};
use core::{convert::Infallible, fmt::Debug};

extern crate static_assertions as sa;

#[allow(dead_code)]
const WORD_BITS: usize = 64;
const TAG_BITS: usize = 3;
const TAG_MASK: u64 = (1u64 << TAG_BITS) - 1;
sa::const_assert!(TAG_MASK == 0x7);

#[allow(dead_code)]
pub(crate) const BODY_BITS: usize = WORD_BITS - (TAG_BITS + 1);
sa::const_assert!(BODY_BITS == 60);

#[allow(dead_code)]
const MAJOR_BITS: usize = 32;
#[allow(dead_code)]
const MINOR_BITS: usize = 28;
#[allow(dead_code)]
const MAJOR_MASK: u64 = (1u64 << MAJOR_BITS) - 1;
const MINOR_MASK: u64 = (1u64 << MINOR_BITS) - 1;
sa::const_assert!(MAJOR_MASK == 0xffff_ffff);
sa::const_assert!(MINOR_MASK == 0x0fff_ffff);
sa::const_assert!(MAJOR_BITS + MINOR_BITS == BODY_BITS);

/// Code values for the 3 "tag" bits in the bit-packed representation
/// of [RawVal].
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Tag {
    /// Tag for a [RawVal] that contains a [u32] number.
    U32 = 0,

    /// Tag for a [RawVal] that contains an [i32] number.
    I32 = 1,

    /// Tag for a [RawVal] that contains a "static" value like `true`, `false`, `void`; see [Static](crate::Static).
    Static = 2,

    /// Tag for a [RawVal] that contains a host object handle; see [Object](crate::Object).
    Object = 3,

    /// Tag for a [RawVal] that contains a symbol; see [Symbol](crate::Symbol).
    Symbol = 4,

    /// Tag for a [RawVal] that contains a small bitset; see [BitSet](crate::BitSet).
    BitSet = 5,

    /// Tag for a [RawVal] that contains a status code; see [Status](crate::Status).
    Status = 6,

    /// Reserved tag for future use.
    #[allow(dead_code)]
    Reserved = 7,
}

impl Tag {
    pub const fn rawval_const(&self) -> i64 {
        ((*self as i64) << 1) | 1
    }
    pub const fn rawval_mask() -> i64 {
        ((TAG_MASK as i64) << 1) | 1
    }
}

sa::const_assert!(Tag::I32.rawval_const() == 3);
sa::const_assert!(Tag::rawval_mask() == 0xf);

/// A 64-bit value encoding a bit-packed disjoint union between several
/// different types (numbers, booleans, symbols, object handles, etc.)
///
/// RawVals divide up the space of 64 bits according to a 2-level tagging
/// scheme. The first tag is a bit in the least-significant position, indicating
/// whether the `RawVal` is a plain "u63" 63-bit unsigned integer, or some
/// more-structured value with a second-level tag in the next most significant 3
/// bits. The 63-bit unsigned integer case can also be thought of as handling
/// the complete range of non-negative signed 64-bit integers.
///
/// The remaining 3 bit tags are assigned to cases enumerated in [Tag], of
/// which 7 are defined and one is currently reserved.
///
/// Schematically, the bit-assignment for `RawVal` looks like this:
///
/// ```text
///    0x_NNNN_NNNN_NNNN_NNNX  - u63, for any even X
///    0x_0000_000N_NNNN_NNN1  - u32
///    0x_0000_000N_NNNN_NNN3  - i32
///    0x_NNNN_NNNN_NNNN_NNN5  - static: void, true, false, ...
///    0x_IIII_IIII_TTTT_TTT7  - object: 32-bit index I, 28-bit type code T
///    0x_NNNN_NNNN_NNNN_NNN9  - symbol: up to 10 6-bit identifier characters
///    0x_NNNN_NNNN_NNNN_NNNb  - bitset: up to 60 bits
///    0x_CCCC_CCCC_TTTT_TTTd  - status: 32-bit code C, 28-bit type code T
///    0x_NNNN_NNNN_NNNN_NNNf  - reserved
/// ```

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct RawVal(u64);

impl Default for RawVal {
    fn default() -> Self {
        Self::from_void()
    }
}

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

impl<E: Env> FromVal<E, RawVal> for RawVal {
    fn from_val(_env: &E, val: RawVal) -> Self {
        val
    }
}

impl<E: Env> FromVal<E, &RawVal> for RawVal {
    fn from_val(_env: &E, val: &RawVal) -> Self {
        *val
    }
}

impl<E: Env> TryFromVal<E, RawVal> for RawVal {
    type Error = Infallible;
    fn try_from_val(_env: &E, val: RawVal) -> Result<Self, Self::Error> {
        Ok(val)
    }
}

impl<E: Env> TryFromVal<E, &RawVal> for RawVal {
    type Error = Infallible;
    fn try_from_val(_env: &E, val: &RawVal) -> Result<Self, Self::Error> {
        Ok(*val)
    }
}

impl<E: Env> IntoVal<E, RawVal> for RawVal {
    fn into_val(self, _env: &E) -> RawVal {
        self
    }
}

impl<E: Env> IntoVal<E, RawVal> for &RawVal {
    fn into_val(self, _env: &E) -> RawVal {
        *self
    }
}

impl<E: Env> TryIntoVal<E, RawVal> for RawVal {
    type Error = Infallible;
    fn try_into_val(self, _env: &E) -> Result<RawVal, Self::Error> {
        Ok(self)
    }
}

impl<E: Env> TryIntoVal<E, RawVal> for &RawVal {
    type Error = Infallible;
    fn try_into_val(self, _env: &E) -> Result<RawVal, Self::Error> {
        Ok(*self)
    }
}

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
/// more efficient conversion code. An implementation of `TryFrom<RawVal>` is also
/// provided for any type that implements `RawValConvertible`.
pub trait RawValConvertible: Into<RawVal> + TryFrom<RawVal> {
    /// Returns `true` if `v` is in a union state compatible with `Self`.
    fn is_val_type(v: RawVal) -> bool;

    /// Converts the bits making up a `RawVal` into `Self` _without_ checking
    /// that the `RawVal` is tagged correctly, assuming that such a check has
    /// been performed elsewhere. It is the caller's responsibility to arrange
    /// that such checks have occurred before calling `unchecked_from_val`,
    /// which is why it is marked as `unsafe` (it does not represent a risk of
    /// memory-unsafety, merely "serious logic errors").
    unsafe fn unchecked_from_val(v: RawVal) -> Self;

    /// Attempt a conversion from `RawVal` to `Self`, returning `None` if the
    /// provided `RawVal` is not tagged correctly. By default this calls
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

// Orphan rules mean we have to macro these, can't blanket-impl on V:Valtype.
macro_rules! declare_tryfrom {
    ($T:ty) => {
        impl TryFrom<RawVal> for $T {
            type Error = ConversionError;
            #[inline(always)]
            fn try_from(v: RawVal) -> Result<Self, Self::Error> {
                if let Some(c) = <Self as RawValConvertible>::try_convert(v) {
                    Ok(c)
                } else {
                    Err(ConversionError)
                }
            }
        }
        impl TryFrom<&RawVal> for $T {
            type Error = ConversionError;
            #[inline(always)]
            fn try_from(v: &RawVal) -> Result<Self, Self::Error> {
                if let Some(c) = <Self as RawValConvertible>::try_convert(*v) {
                    Ok(c)
                } else {
                    Err(ConversionError)
                }
            }
        }
        impl<E: Env> IntoVal<E, RawVal> for $T {
            fn into_val(self, _env: &E) -> RawVal {
                self.into()
            }
        }
        impl<E: Env> IntoVal<E, RawVal> for &$T {
            fn into_val(self, _env: &E) -> RawVal {
                (*self).into()
            }
        }
        impl<E: Env> TryFromVal<E, RawVal> for $T {
            type Error = ConversionError;
            #[inline(always)]
            fn try_from_val(_env: &E, val: RawVal) -> Result<Self, Self::Error> {
                Self::try_from(val)
            }
        }
        impl<E: Env> TryIntoVal<E, RawVal> for $T {
            type Error = ConversionError;
            fn try_into_val(self, _env: &E) -> Result<RawVal, Self::Error> {
                Ok(self.into())
            }
        }
        impl<E: Env> TryIntoVal<E, $T> for RawVal {
            type Error = ConversionError;
            fn try_into_val(self, env: &E) -> Result<$T, Self::Error> {
                <_ as TryFromVal<E, RawVal>>::try_from_val(&env, self)
            }
        }
    };
}

declare_tryfrom!(());
declare_tryfrom!(bool);
declare_tryfrom!(u32);
declare_tryfrom!(i32);
declare_tryfrom!(BitSet);
declare_tryfrom!(Status);
declare_tryfrom!(Symbol);
declare_tryfrom!(Static);
declare_tryfrom!(Object);

#[cfg(feature = "vm")]
impl wasmi::core::FromValue for RawVal {
    fn from_value(val: wasmi::core::Value) -> Option<Self> {
        if let wasmi::core::Value::I64(i) = val {
            Some(RawVal::from_payload(i as u64))
        } else {
            None
        }
    }
}

#[cfg(feature = "vm")]
impl From<RawVal> for wasmi::core::Value {
    fn from(v: RawVal) -> Self {
        wasmi::core::Value::I64(v.get_payload() as i64)
    }
}

impl RawValConvertible for () {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::Static) && v.get_body() == ScStatic::Void as u64
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(_v: RawVal) -> Self {}
}

impl RawValConvertible for bool {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::Static)
            && (v.get_body() == ScStatic::True as u64 || v.get_body() == ScStatic::False as u64)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        v.get_body() == ScStatic::True as u64
    }
    #[inline(always)]
    fn try_convert(v: RawVal) -> Option<Self> {
        if v.has_tag(Tag::Static) {
            if v.get_body() == ScStatic::True as u64 {
                Some(true)
            } else if v.get_body() == ScStatic::False as u64 {
                Some(false)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl RawValConvertible for u32 {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::U32)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        v.get_body() as u32
    }
}

impl RawValConvertible for i32 {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::I32)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        v.get_body() as i32
    }
}

impl From<bool> for RawVal {
    #[inline(always)]
    fn from(b: bool) -> Self {
        RawVal::from_bool(b)
    }
}

impl From<()> for RawVal {
    #[inline(always)]
    fn from(_: ()) -> Self {
        RawVal::from_void()
    }
}

impl From<&()> for RawVal {
    #[inline(always)]
    fn from(_: &()) -> Self {
        RawVal::from_void()
    }
}

impl From<u32> for RawVal {
    #[inline(always)]
    fn from(u: u32) -> Self {
        RawVal::from_u32(u)
    }
}

impl From<&u32> for RawVal {
    #[inline(always)]
    fn from(u: &u32) -> Self {
        RawVal::from_u32(*u)
    }
}

impl From<i32> for RawVal {
    #[inline(always)]
    fn from(i: i32) -> Self {
        RawVal::from_i32(i)
    }
}

impl From<&i32> for RawVal {
    #[inline(always)]
    fn from(i: &i32) -> Self {
        RawVal::from_i32(*i)
    }
}

impl From<ScStatus> for RawVal {
    fn from(st: ScStatus) -> Self {
        let ty = st.discriminant();
        let code = match st {
            ScStatus::Ok => ScStatusType::Ok as u32,
            ScStatus::UnknownError(e) => e as u32,
            ScStatus::HostValueError(e) => e as u32,
            ScStatus::HostObjectError(e) => e as u32,
            ScStatus::HostFunctionError(e) => e as u32,
            ScStatus::HostStorageError(e) => e as u32,
            ScStatus::HostContextError(e) => e as u32,
            ScStatus::VmError(e) => e as u32,
            ScStatus::ContractError(e) => e,
        };
        Status::from_type_and_code(ty, code).to_raw()
    }
}

impl From<&ScStatus> for RawVal {
    fn from(st: &ScStatus) -> Self {
        let ty = st.discriminant();
        let code = match *st {
            ScStatus::Ok => ScStatusType::Ok as u32,
            ScStatus::UnknownError(e) => e as u32,
            ScStatus::HostValueError(e) => e as u32,
            ScStatus::HostObjectError(e) => e as u32,
            ScStatus::HostFunctionError(e) => e as u32,
            ScStatus::HostStorageError(e) => e as u32,
            ScStatus::HostContextError(e) => e as u32,
            ScStatus::VmError(e) => e as u32,
            ScStatus::ContractError(e) => e,
        };
        Status::from_type_and_code(ty, code).to_raw()
    }
}

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
    pub const fn is_u63(self) -> bool {
        (self.0 & 1) == 0
    }

    #[inline(always)]
    pub const unsafe fn unchecked_as_u63(self) -> i64 {
        (self.0 >> 1) as i64
    }

    #[inline(always)]
    pub const unsafe fn unchecked_from_u63(i: i64) -> Self {
        Self((i as u64) << 1)
    }

    #[inline(always)]
    const fn get_tag_u8(self) -> u8 {
        ((self.0 >> 1) & TAG_MASK) as u8
    }

    #[inline(always)]
    pub const fn get_tag(self) -> Tag {
        unsafe { ::core::mem::transmute(self.get_tag_u8()) }
    }

    #[inline(always)]
    pub(crate) const fn get_body(self) -> u64 {
        self.0 >> (TAG_BITS + 1)
    }

    #[inline(always)]
    pub(crate) const fn has_tag(self, tag: Tag) -> bool {
        !self.is_u63() && self.get_tag_u8() == tag as u8
    }

    #[inline(always)]
    pub fn is<T: RawValConvertible>(self) -> bool {
        T::is_val_type(self)
    }

    #[inline(always)]
    // This does no checking, so it can be used in const fns
    // below; it should not be made public.
    pub(crate) const unsafe fn from_body_and_tag(body: u64, tag: Tag) -> RawVal {
        let body = (body << TAG_BITS) | (tag as u64);
        RawVal((body << 1) | 1)
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
    pub(crate) const fn get_minor(self) -> u32 {
        (self.get_body() & MINOR_MASK) as u32
    }

    #[inline(always)]
    pub(crate) const fn get_major(self) -> u32 {
        (self.get_body() >> MINOR_BITS) as u32
    }

    #[inline(always)]
    pub const fn from_void() -> RawVal {
        unsafe { RawVal::from_body_and_tag(ScStatic::Void as u64, Tag::Static) }
    }

    #[inline(always)]
    pub const fn from_bool(b: bool) -> RawVal {
        let body = if b { ScStatic::True } else { ScStatic::False };
        unsafe { RawVal::from_body_and_tag(body as u64, Tag::Static) }
    }

    #[inline(always)]
    pub const fn from_other_static(st: ScStatic) -> RawVal {
        unsafe { RawVal::from_body_and_tag(st as u64, Tag::Static) }
    }

    #[inline(always)]
    pub const fn from_u32(u: u32) -> RawVal {
        unsafe { RawVal::from_body_and_tag(u as u64, Tag::U32) }
    }

    #[inline(always)]
    pub const fn from_i32(i: i32) -> RawVal {
        unsafe { RawVal::from_body_and_tag((i as u32) as u64, Tag::I32) }
    }

    #[inline(always)]
    pub const fn is_i32_zero(self) -> bool {
        self.0 == Self::I32_ZERO.0
    }

    #[inline(always)]
    pub const fn is_u32_zero(self) -> bool {
        self.0 == Self::U32_ZERO.0
    }

    #[inline(always)]
    pub const fn is_void(self) -> bool {
        self.0 == Self::VOID.0
    }

    #[inline(always)]
    pub const fn is_true(self) -> bool {
        self.0 == Self::TRUE.0
    }

    #[inline(always)]
    pub const fn is_false(self) -> bool {
        self.0 == Self::FALSE.0
    }
}

impl RawVal {
    pub const I32_ZERO: RawVal = RawVal::from_i32(0);
    pub const I32_POSITIVE_ONE: RawVal = RawVal::from_i32(1);
    pub const I32_NEGATIVE_ONE: RawVal = RawVal::from_i32(-1);
    pub const I32_MIN: RawVal = RawVal::from_i32(i32::MIN);
    pub const I32_MAX: RawVal = RawVal::from_i32(i32::MAX);

    pub const U32_ZERO: RawVal = RawVal::from_u32(0);
    pub const U32_ONE: RawVal = RawVal::from_u32(1);
    pub const U32_MIN: RawVal = RawVal::from_u32(u32::MIN);
    pub const U32_MAX: RawVal = RawVal::from_u32(u32::MAX);

    pub const VOID: RawVal = RawVal::from_other_static(ScStatic::Void);

    pub const TRUE: RawVal = RawVal::from_bool(true);
    pub const FALSE: RawVal = RawVal::from_bool(false);
}

impl Debug for RawVal {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.is_u63() {
            write!(f, "U63({})", unsafe { self.unchecked_as_u63() })
        } else {
            match self.get_tag() {
                Tag::U32 => write!(f, "U32({})", self.get_body() as u32),
                Tag::I32 => write!(f, "I32({})", self.get_body() as i32),
                Tag::Static => {
                    let scs = ScStatic::try_from(self.get_body() as i32);
                    if let Ok(scs) = scs {
                        write!(f, "{}", scs.name())
                    } else {
                        write!(f, "Static({})", self.get_body())
                    }
                }
                Tag::Object => {
                    unsafe { <Object as RawValConvertible>::unchecked_from_val(*self) }.fmt(f)
                }
                Tag::Symbol => {
                    unsafe { <Symbol as RawValConvertible>::unchecked_from_val(*self) }.fmt(f)
                }
                Tag::BitSet => {
                    unsafe { <BitSet as RawValConvertible>::unchecked_from_val(*self) }.fmt(f)
                }
                Tag::Status => {
                    unsafe { <Status as RawValConvertible>::unchecked_from_val(*self) }.fmt(f)
                }
                Tag::Reserved => {
                    write!(f, "Reserved({})", self.get_body())
                }
            }
        }
    }
}

#[test]
#[cfg(feature = "std")]
fn test_debug() {
    use super::{BitSet, Object, Status, Symbol};
    use crate::xdr::{ScHostValErrorCode, ScObjectType, ScStatus};
    assert_eq!(format!("{:?}", RawVal::from_void()), "Void");
    assert_eq!(format!("{:?}", RawVal::from_bool(true)), "True");
    assert_eq!(format!("{:?}", RawVal::from_bool(false)), "False");
    assert_eq!(format!("{:?}", RawVal::from_i32(10)), "I32(10)");
    assert_eq!(format!("{:?}", RawVal::from_u32(10)), "U32(10)");
    assert_eq!(
        format!("{:?}", unsafe { RawVal::unchecked_from_u63(10) }),
        "U63(10)"
    );
    assert_eq!(
        format!("{:?}", BitSet::try_from_u64(0xf7).unwrap().as_raw()),
        "BitSet(0b11110111)"
    );
    assert_eq!(format!("{:?}", Symbol::from_str("hello")), "Symbol(hello)");
    assert_eq!(
        format!("{:?}", Object::from_type_and_handle(ScObjectType::Vec, 7)),
        "Object(Vec(#7))"
    );
    assert_eq!(
        format!(
            "{:?}",
            Status::from_status(ScStatus::HostValueError(
                ScHostValErrorCode::ReservedTagValue
            ),)
        ),
        "Status(HostValueError(ReservedTagValue))"
    );
}
