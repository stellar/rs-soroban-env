use stellar_xdr::{ScStatic, ScStatus, ScStatusType};

use super::{BitSet, Env, EnvVal, Object, Status, Symbol};
use core::fmt::Debug;

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

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Tag {
    U32 = 0,
    I32 = 1,
    Static = 2,
    Object = 3,
    Symbol = 4,
    BitSet = 5,
    Status = 6,

    #[allow(dead_code)]
    Reserved = 7,
}

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

// This is a 0-arg struct rather than an enum to ensure it completely compiles
// away, the same way `()` would, while remaining a separate type to allow
// conversion to a more-structured error code at a higher level.
#[derive(Debug, Eq, PartialEq)]
pub struct ConversionError;

impl From<stellar_xdr::Error> for ConversionError {
    fn from(_: stellar_xdr::Error) -> Self {
        ConversionError
    }
}

pub trait RawValConvertible: Into<RawVal> + TryFrom<RawVal> {
    fn is_val_type(v: RawVal) -> bool;
    unsafe fn unchecked_from_val(v: RawVal) -> Self;

    // Try_convert has a default implementation that is
    // test-and-unchecked-convert, but also allows us to customize its
    // implementation for types in which that would produce an undesirable
    // replication of tests.
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
        impl<E: Env> TryFrom<EnvVal<E, RawVal>> for $T {
            type Error = ConversionError;
            #[inline(always)]
            fn try_from(v: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
                Self::try_from(v.to_raw())
            }
        }
    };
}

declare_tryfrom!(());
declare_tryfrom!(bool);
declare_tryfrom!(u32);
declare_tryfrom!(i32);
declare_tryfrom!(BitSet);

#[cfg(feature = "vm")]
impl wasmi::FromValue for RawVal {
    fn from_value(val: wasmi::RuntimeValue) -> Option<Self> {
        let maybe: Option<u64> = val.try_into();
        maybe.map(RawVal::from_payload)
    }
}

#[cfg(feature = "vm")]
impl From<RawVal> for wasmi::RuntimeValue {
    fn from(v: RawVal) -> Self {
        wasmi::RuntimeValue::I64(v.get_payload() as i64)
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
        };
        Status::from_type_and_code(ty, code).to_raw()
    }
}

impl From<u8> for RawVal {
    #[inline(always)]
    fn from(u: u8) -> Self {
        RawVal::from_u32(u.into())
    }
}

impl RawVal {
    pub fn in_env<E: Env>(self, env: &E) -> EnvVal<E, RawVal> {
        EnvVal {
            env: env.clone(),
            val: self,
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
    pub const fn is_u32_zero(self) -> bool {
        const ZERO: RawVal = RawVal::from_u32(0);
        self.0 == ZERO.0
    }

    #[inline(always)]
    pub const fn is_void(self) -> bool {
        const VOID: RawVal = RawVal::from_other_static(ScStatic::Void);
        self.0 == VOID.0
    }

    #[inline(always)]
    pub const fn is_true(self) -> bool {
        const TRUE: RawVal = RawVal::from_bool(true);
        self.0 == TRUE.0
    }

    #[inline(always)]
    pub const fn is_false(self) -> bool {
        const FALSE: RawVal = RawVal::from_bool(false);
        self.0 == FALSE.0
    }
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
        "Object(Vec(7))"
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
