// This permits globals prouced by derive(num_enum::TryFromPrimitive) below.
#![cfg_attr(test, allow(non_upper_case_globals))]

use crate::xdr::{ScError, ScVal, ScValType};
use crate::{
    declare_tag_based_object_wrapper, declare_tag_based_wrapper,
    impl_tryfroms_and_tryfromvals_delegating_to_valconvert, impl_val_wrapper_base, Compare, I32Val,
    SymbolSmall, SymbolStr, U32Val,
};

use super::{Env, Error, TryFromVal};
use core::{cmp::Ordering, convert::Infallible, fmt::Debug, str};

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
/// of [Val]. These don't coincide with tag numbers in the SCVal XDR
/// but cover all those cases as well as some optimized refinements for
/// special cases (boolean true and false, small-value forms).
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(test, derive(num_enum::TryFromPrimitive))]
pub enum Tag {
    /// Tag for a [Val] that encodes [bool] `false`. The bool type is refined to
    /// two single-value subtypes in order for each tag number to coincide with
    /// the WASM encoding of a boolean.
    False = 0,

    /// Tag for a [Val] that encodes [bool] `true`.
    True = 1,

    /// Tag for a [Val] that is empty/absent (eg. void, null, nil, undefined, None)
    Void = 2,

    /// Tag for a [Val] that is contains an error code.
    Error = 3,

    /// Tag for a [Val] that contains a [u32] number.
    U32Val = 4,

    /// Tag for a [Val] that contains an [i32] number.
    I32Val = 5,

    /// Tag for a [Val] that contains a [u64] small enough to fit in 56 bits.
    U64Small = 6,

    /// Tag for a [Val] that contains an [i64] small enough to fit in 56 bits.
    I64Small = 7,

    /// Tag for a [Val] that contains a [u64] timepoint small enough to fit
    /// in 56 bits.
    TimepointSmall = 8,

    /// Tag for a [Val] that contains a [u64] duration small enough to fit in
    /// 56 bits.
    DurationSmall = 9,

    /// Tag for a [Val] that contains a [u128] small enough to fit in 56 bits.
    U128Small = 10,

    /// Tag for a [Val] that contains an [i128] small enough to fit in 56 bits.
    I128Small = 11,

    /// Tag for a [Val] that contains a [u256](ethnum::u256) small enough to fit in 56 bits.
    U256Small = 12,

    /// Tag for a [Val] that contains an [i256](ethnum::i256) small enough to fit in 56 bits.
    I256Small = 13,

    /// Tag for a [Val] that contains up to 9 character symbols.
    SymbolSmall = 14,

    /// Code delimiting the upper boundary of "small" types.
    SmallCodeUpperBound = 15,

    /// Tag reserved to indicate boundary between tags for "small" types with
    /// their payload packed into the remaining 56 bits of the [Val] and
    /// "object" types that are stored as host objects and referenced by
    /// [Object](crate::Object) handle.
    ObjectCodeLowerBound = 63,

    /// Tag for a [Val] that refers to a host-side [u64] number.
    U64Object = 64,

    /// Tag for a [Val] that refers to a host-side [i64] number.
    I64Object = 65,

    /// Tag for a [Val] that refers to a host-side [u64] number encoding a
    /// time-point (a count of seconds since the Unix epoch, Jan 1 1970 UTC).
    TimepointObject = 66,

    /// Tag for a [Val] that refers to a host-side [i64] number encoding a
    /// duration (a count of seconds).
    DurationObject = 67,

    /// Tag for a [Val] that refers to a host-side [u128] number.
    U128Object = 68,

    /// Tag for a [Val] that refers to a host-side [i128] number.
    I128Object = 69,

    /// Tag for a [Val] that refers to a host-side [u256](ethnum::u256) number.
    U256Object = 70,

    /// Tag for a [Val] that refers to a host-side [i256](ethnum::i256) number.
    I256Object = 71,

    /// Tag for a [Val] that refers to a host-side byte array.
    BytesObject = 72,

    /// Tag for a [Val] that refers to a host-side string.
    StringObject = 73,

    /// Tag for a [Val] that refers to a host-side symbol (see [`Symbol`](crate::Symbol)).
    SymbolObject = 74,

    /// Tag for a [Val] that refers to a host-side vector of [Val]s.
    VecObject = 75,

    /// Tag for a [Val] that refers to a host-side map from [Val]s to [Val]s.
    MapObject = 76,

    /// Tag for a [Val] that refers to a host-side contract address.
    AddressObject = 77,

    /// Code delimiting the upper boundary of "object" types.
    ObjectCodeUpperBound = 78,

    /// Code reserved to indicate mis-tagged [`Val`]s.
    Bad = 0x7f,
}

impl Tag {
    #[inline(always)]
    pub const fn val_mask() -> i64 {
        TAG_MASK as i64
    }

    #[inline(always)]
    pub fn val_const(&self) -> i64 {
        *self as i64
    }

    #[inline(always)]
    pub(crate) const fn u8_is_object(x: u8) -> bool {
        x > (Tag::ObjectCodeLowerBound as u8) && x < (Tag::ObjectCodeUpperBound as u8)
    }

    #[inline(always)]
    pub const fn is_object(self) -> bool {
        Self::u8_is_object(self as u8)
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
            Tag::AddressObject => Some(ScValType::Address),
            Tag::ObjectCodeUpperBound => None,
            Tag::Bad => None,
        }
    }
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Val(u64);

impl Default for Val {
    fn default() -> Self {
        Self::from_void().into()
    }
}

// Impl AsRef/AsMut and TryFromVal<Val> so that clients can abstract over a
// wrapper-or-Val because all wrappers also impl these.
impl AsRef<Val> for Val {
    fn as_ref(&self) -> &Val {
        self
    }
}

impl AsMut<Val> for Val {
    fn as_mut(&mut self) -> &mut Val {
        self
    }
}

impl<E: Env> TryFromVal<E, Val> for Val {
    type Error = ConversionError;
    fn try_from_val(_env: &E, val: &Val) -> Result<Self, Self::Error> {
        Ok(*val)
    }
}

// Declare a few extra small-value wrapper types that don't live anywhere else.
declare_tag_based_wrapper!(Void);

impl From<()> for Void {
    fn from(_value: ()) -> Self {
        Val::VOID
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
pub struct Bool(Val);
impl_val_wrapper_base!(Bool);

impl From<bool> for Bool {
    fn from(value: bool) -> Self {
        Val::from_bool(value)
    }
}
impl From<Bool> for bool {
    fn from(value: Bool) -> Self {
        value.0.is_true()
    }
}

impl ValConvert for Bool {
    fn is_val_type(v: Val) -> bool {
        v.is_true() || v.is_false()
    }

    unsafe fn unchecked_from_val(v: Val) -> Self {
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
declare_tag_based_object_wrapper!(AddressObject);

// This is a 0-arg struct rather than an enum to ensure it completely compiles
// away, the same way `()` would, while remaining a separate type to allow
// conversion to a more-structured error code at a higher level.

/// Error type indicating a failure to convert some type to another; details of
/// the failed conversion may be written to the debug log, when possible.
///
/// This is intentionally minimal and uninformative to minimize impact of its
/// use on wasm codesize. It converts to `Error(ScErrorType::Value,
/// ScErrorCode::UnexpectedType)` when converted to a full `Error`, and ideally
/// it should only be used in ubiquitous cases that will occur in wasm, like
/// small-number or tag conversions, where code size is paramount and the
/// information-loss from using it is not too bad.
#[derive(Debug, Eq, PartialEq)]
pub struct ConversionError;

impl From<Infallible> for ConversionError {
    fn from(_: Infallible) -> Self {
        unreachable!()
    }
}

impl From<crate::xdr::Error> for ConversionError {
    fn from(_: crate::xdr::Error) -> Self {
        ConversionError
    }
}

impl From<crate::Error> for ConversionError {
    fn from(_: crate::Error) -> Self {
        ConversionError
    }
}

/// Trait abstracting over types that can be converted into [Val], similar to
/// [TryFrom] but with a different signature that enables generating slightly
/// more efficient conversion code.
pub(crate) trait ValConvert: Into<Val> + TryFrom<Val> {
    /// Returns `true` if `v` is in a union state compatible with `Self`.
    fn is_val_type(v: Val) -> bool;

    /// Converts the bits making up a `Val` into `Self` _without_ checking
    /// that the `Val` is tagged correctly, assuming that such a check has
    /// been performed elsewhere. It is the caller's responsibility to arrange
    /// that such checks have occurred before calling `unchecked_from_val`,
    /// which is why it is marked as `unsafe` (it does not represent a risk of
    /// memory-unsafety, merely "serious logic errors").
    unsafe fn unchecked_from_val(v: Val) -> Self;

    /// Attempt a conversion from `Val` to `Self`, returning `None` if the
    /// provided `Val` is not tagged correctly. By default this calls
    /// `Self::is_val_type` and `Self::unchecked_from_val`, but it can be
    /// customized on a type-by-type basis to avoid redundant tag tests and
    /// produce more efficient code, as it is done for `Static` values like
    /// `bool`.
    #[inline(always)]
    fn try_convert(v: Val) -> Option<Self> {
        if Self::is_val_type(v) {
            Some(unsafe { Self::unchecked_from_val(v) })
        } else {
            None
        }
    }
}

impl_tryfroms_and_tryfromvals_delegating_to_valconvert!(());
impl_tryfroms_and_tryfromvals_delegating_to_valconvert!(bool);
impl_tryfroms_and_tryfromvals_delegating_to_valconvert!(u32);
impl_tryfroms_and_tryfromvals_delegating_to_valconvert!(i32);
impl_tryfroms_and_tryfromvals_delegating_to_valconvert!(Error);

#[cfg(feature = "wasmi")]
pub trait WasmiMarshal: Sized {
    fn try_marshal_from_value(v: wasmi::Value) -> Option<Self>;
    fn marshal_from_self(self) -> wasmi::Value;
}

#[cfg(feature = "wasmi")]
impl WasmiMarshal for Val {
    fn try_marshal_from_value(v: wasmi::Value) -> Option<Self> {
        if let wasmi::Value::I64(i) = v {
            let v = Val::from_payload(i as u64);
            if v.is_good() {
                Some(v)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn marshal_from_self(self) -> wasmi::Value {
        wasmi::Value::I64(self.get_payload() as i64)
    }
}

#[cfg(feature = "wasmi")]
impl WasmiMarshal for u64 {
    fn try_marshal_from_value(v: wasmi::Value) -> Option<Self> {
        if let wasmi::Value::I64(i) = v {
            Some(i as u64)
        } else {
            None
        }
    }

    fn marshal_from_self(self) -> wasmi::Value {
        wasmi::Value::I64(self as i64)
    }
}

#[cfg(feature = "wasmi")]
impl WasmiMarshal for i64 {
    fn try_marshal_from_value(v: wasmi::Value) -> Option<Self> {
        if let wasmi::Value::I64(i) = v {
            Some(i)
        } else {
            None
        }
    }

    fn marshal_from_self(self) -> wasmi::Value {
        wasmi::Value::I64(self)
    }
}

// Manually implement all the residual pieces: ValConverts
// and Froms.

impl ValConvert for () {
    #[inline(always)]
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::Void)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(_v: Val) -> Self {}
}

impl ValConvert for bool {
    #[inline(always)]
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::True) || v.has_tag(Tag::False)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: Val) -> Self {
        v.has_tag(Tag::True)
    }
    #[inline(always)]
    fn try_convert(v: Val) -> Option<Self> {
        if v.has_tag(Tag::True) {
            Some(true)
        } else if v.has_tag(Tag::False) {
            Some(false)
        } else {
            None
        }
    }
}

impl ValConvert for u32 {
    #[inline(always)]
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::U32Val)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: Val) -> Self {
        v.get_major()
    }
}

impl ValConvert for i32 {
    #[inline(always)]
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::I32Val)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: Val) -> Self {
        v.get_major() as i32
    }
}

impl From<bool> for Val {
    #[inline(always)]
    fn from(b: bool) -> Self {
        Val::from_bool(b).into()
    }
}

impl From<()> for Val {
    #[inline(always)]
    fn from(_: ()) -> Self {
        Val::from_void().into()
    }
}

impl From<&()> for Val {
    #[inline(always)]
    fn from(_: &()) -> Self {
        Val::from_void().into()
    }
}

impl From<u32> for Val {
    #[inline(always)]
    fn from(u: u32) -> Self {
        Val::from_u32(u).into()
    }
}

impl From<&u32> for Val {
    #[inline(always)]
    fn from(u: &u32) -> Self {
        Val::from_u32(*u).into()
    }
}

impl From<i32> for Val {
    #[inline(always)]
    fn from(i: i32) -> Self {
        Val::from_i32(i).into()
    }
}

impl From<&i32> for Val {
    #[inline(always)]
    fn from(i: &i32) -> Self {
        Val::from_i32(*i).into()
    }
}

impl From<ScError> for Val {
    fn from(er: ScError) -> Self {
        let e: Error = er.into();
        e.to_val()
    }
}

impl From<&ScError> for Val {
    fn from(er: &ScError) -> Self {
        let e: Error = er.clone().into();
        e.to_val()
    }
}

// Utility methods

impl Val {
    /// Some ScVals are not representable as Vals at all,
    /// and only exist in the XDR to serve as special storage
    /// system key or value payloads managed by the Host.
    pub const fn can_represent_scval_type(scv_ty: ScValType) -> bool {
        match scv_ty {
            ScValType::Bool
            | ScValType::Void
            | ScValType::Error
            | ScValType::U32
            | ScValType::I32
            | ScValType::U64
            | ScValType::I64
            | ScValType::Timepoint
            | ScValType::Duration
            | ScValType::U128
            | ScValType::I128
            | ScValType::U256
            | ScValType::I256
            | ScValType::Bytes
            | ScValType::String
            | ScValType::Symbol
            | ScValType::Vec
            | ScValType::Map
            | ScValType::Address => true,
            ScValType::ContractInstance
            | ScValType::LedgerKeyContractInstance
            | ScValType::LedgerKeyNonce => false,
        }
    }

    /// *Non-recursively* checks whether `ScVal` can be represented as `Val`.
    /// Since conversions from `ScVal` are recursive themselves, this should
    /// be called at every recursion level during conversion.
    pub fn can_represent_scval(scv: &ScVal) -> bool {
        match scv {
            // Map/vec can't be validated just based on the discriminant,
            // as their contents can't be `None`.
            ScVal::Vec(None) => return false,
            ScVal::Map(None) => return false,
            _ => Self::can_represent_scval_type(scv.discriminant()),
        }
    }

    /// *Recursively* checks whether `ScVal` can be represented as `Val`.
    /// This should only be used once per top-level `ScVal`.
    pub fn can_represent_scval_recursive(scv: &ScVal) -> bool {
        match scv {
            // Handle recursive types first
            ScVal::Vec(None) => return false,
            ScVal::Map(None) => return false,
            ScVal::Vec(Some(v)) => {
                return v.0.iter().all(|x| Val::can_represent_scval_recursive(x))
            }
            ScVal::Map(Some(m)) => {
                return m.0.iter().all(|e| {
                    Val::can_represent_scval_recursive(&e.key)
                        && Val::can_represent_scval_recursive(&e.val)
                })
            }
            _ => Self::can_represent_scval_type(scv.discriminant()),
        }
    }

    /// We define a "good" Val as one that has one of the allowed tag values,
    /// all the defined body-bits for its case set to valid values, and all the
    /// undefined body-bits set to zero.
    pub fn is_good(self) -> bool {
        match self.get_tag() {
            // Technically Tag::Bad is the only one that can occur here -- the other
            // 3 are mapped to it -- but we check for them just in case.
            Tag::Bad
            | Tag::SmallCodeUpperBound
            | Tag::ObjectCodeLowerBound
            | Tag::ObjectCodeUpperBound => false,
            Tag::True | Tag::False | Tag::Void => self.has_body(0),
            Tag::I32Val | Tag::U32Val => self.has_minor(0),
            Tag::Error => ScError::try_from(unsafe { Error::unchecked_from_val(self) }).is_ok(),
            Tag::SymbolSmall => SymbolSmall::try_from_body(self.get_body()).is_ok(),
            Tag::U64Small
            | Tag::I64Small
            | Tag::TimepointSmall
            | Tag::DurationSmall
            | Tag::U128Small
            | Tag::I128Small
            | Tag::U256Small
            | Tag::I256Small => true,
            Tag::U64Object
            | Tag::I64Object
            | Tag::TimepointObject
            | Tag::DurationObject
            | Tag::U128Object
            | Tag::I128Object
            | Tag::U256Object
            | Tag::I256Object
            | Tag::BytesObject
            | Tag::StringObject
            | Tag::SymbolObject
            | Tag::VecObject
            | Tag::MapObject
            | Tag::AddressObject => self.has_minor(0),
        }
    }

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
    pub(crate) const fn has_body(self, body: u64) -> bool {
        self.get_body() == body
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
    // This does no checking, so it can be used in const fns
    // below; it should not be made public.
    pub(crate) const unsafe fn from_body_and_tag(body: u64, tag: Tag) -> Val {
        Val((body << TAG_BITS) | (tag as u64))
    }

    #[inline(always)]
    // This also does not checking, is a crate-local helper.
    pub(crate) const unsafe fn from_major_minor_and_tag(major: u32, minor: u32, tag: Tag) -> Val {
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
        Tag::u8_is_object(self.get_tag_u8())
    }

    #[inline(always)]
    pub const fn from_void() -> Void {
        unsafe { Void(Val::from_body_and_tag(0, Tag::Void)) }
    }

    #[inline(always)]
    pub const fn from_bool(b: bool) -> Bool {
        let tag = if b { Tag::True } else { Tag::False };
        unsafe { Bool(Val::from_body_and_tag(0, tag)) }
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

impl Val {
    pub const I32_ZERO: I32Val = Val::from_i32(0);
    pub const I32_MIN: I32Val = Val::from_i32(i32::MIN);
    pub const I32_MAX: I32Val = Val::from_i32(i32::MAX);

    pub const U32_ZERO: U32Val = Val::from_u32(0);
    pub const U32_ONE: U32Val = Val::from_u32(1);
    pub const U32_MIN: U32Val = Val::from_u32(u32::MIN);
    pub const U32_MAX: U32Val = Val::from_u32(u32::MAX);

    pub const VOID: Void = Val::from_void();

    pub const TRUE: Bool = Val::from_bool(true);
    pub const FALSE: Bool = Val::from_bool(false);
}

impl Debug for Val {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        fn fmt_obj(name: &str, r: &Val, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            write!(f, "{}(obj#{})", name, r.get_major())
        }

        match self.get_tag() {
            Tag::U32Val => write!(f, "U32({})", self.get_major()),
            Tag::I32Val => write!(f, "I32({})", self.get_major() as i32),
            Tag::False => write!(f, "False"),
            Tag::True => write!(f, "True"),
            Tag::Void => write!(f, "Void"),
            Tag::Error => unsafe { <Error as ValConvert>::unchecked_from_val(*self) }.fmt(f),
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
                    unsafe { <SymbolSmall as ValConvert>::unchecked_from_val(*self) }.into();
                // Even though this may be called for an arbitrary, not necessarily well-formed
                // `Val`, this is still safe thanks to `SymbolSmall` iteration implementation that
                // only returns valid symbol characters or `\0` even for invalid bit
                // representations.
                let s: &str = ss.as_ref();
                write!(f, "Symbol({})", s)
            }

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
            Tag::AddressObject => fmt_obj("Address", self, f),

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
        xdr::{ScError, ScErrorCode},
        I64Small, U64Small,
    };
    assert_eq!(format!("{:?}", Val::from_void()), "Void");
    assert_eq!(format!("{:?}", Val::from_bool(true)), "True");
    assert_eq!(format!("{:?}", Val::from_bool(false)), "False");
    assert_eq!(format!("{:?}", Val::from_i32(10)), "I32(10)");
    assert_eq!(format!("{:?}", Val::from_i32(-10)), "I32(-10)");
    assert_eq!(format!("{:?}", Val::from_u32(10)), "U32(10)");
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
            Error::from_scerror(ScError::Value(ScErrorCode::InvalidInput))
        ),
        "Error(Value, InvalidInput)"
    );
}

// `Tag::from_u8` is implemented by hand unsafely.
//
// This test ensures that all cases are correct by comparing to the
// macro-generated results of the num_enum crate, which is only enabled as a
// dev-dependency.
#[test]
fn test_tag_from_u8() {
    use num_enum::TryFromPrimitive;

    for i in 0_u8..=255 {
        let expected_tag = Tag::try_from_primitive(i);
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
