use super::{BitSet, Object, Status, Symbol};

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

#[repr(u32)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Static {
    Void = 0,
    True = 1,
    False = 2,
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Val(u64);

pub trait ValType: Into<Val> {
    fn is_val_type(v: Val) -> bool;
    unsafe fn unchecked_from_val(v: Val) -> Self;

    // Try_convert has a default implementation that is
    // test-and-unchecked-convert, but also allows us to customize its
    // implementation for types in which that would produce an undesirable
    // replication of tests.
    #[inline(always)]
    fn try_convert(v: Val) -> Option<Self> {
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
        impl TryFrom<Val> for $T {
            type Error = ();
            #[inline(always)]
            fn try_from(v: Val) -> Result<Self, Self::Error> {
                if let Some(c) = <Self as ValType>::try_convert(v) {
                    Ok(c)
                } else {
                    Err(())
                }
            }
        }
    };
}

impl TryFrom<Val> for i64 {
    type Error = ();

    fn try_from(value: Val) -> Result<Self, Self::Error> {
        if value.is_u63() {
            Ok(unsafe { value.unchecked_as_u63() })
        } else {
            Err(())
        }
    }
}

impl TryFrom<i64> for Val {
    type Error = ();

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        if value >= 0 {
            Ok(unsafe { Val::unchecked_from_u63(value) })
        } else {
            Err(())
        }
    }
}

declare_tryfrom!(());
declare_tryfrom!(bool);
declare_tryfrom!(u32);
declare_tryfrom!(i32);
declare_tryfrom!(Object);
declare_tryfrom!(Symbol);
declare_tryfrom!(BitSet);
declare_tryfrom!(Status);

impl ValType for () {
    #[inline(always)]
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::Static) && v.get_body() == Static::Void as u64
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(_v: Val) -> Self {
        ()
    }
}

impl ValType for bool {
    #[inline(always)]
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::Static)
            && (v.get_body() == Static::True as u64 || v.get_body() == Static::False as u64)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: Val) -> Self {
        v.get_body() == Static::True as u64
    }
    #[inline(always)]
    fn try_convert(v: Val) -> Option<Self> {
        if v.has_tag(Tag::Static) {
            if v.get_body() == Static::True as u64 {
                Some(true)
            } else if v.get_body() == Static::False as u64 {
                Some(false)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl ValType for u32 {
    #[inline(always)]
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::U32)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: Val) -> Self {
        v.get_body() as u32
    }
}

impl ValType for i32 {
    #[inline(always)]
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::I32)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: Val) -> Self {
        v.get_body() as i32
    }
}

impl From<bool> for Val {
    #[inline(always)]
    fn from(b: bool) -> Self {
        Val::from_bool(b)
    }
}

impl From<()> for Val {
    #[inline(always)]
    fn from(_: ()) -> Self {
        Val::from_void()
    }
}

impl From<u32> for Val {
    #[inline(always)]
    fn from(u: u32) -> Self {
        Val::from_u32(u)
    }
}

impl From<i32> for Val {
    #[inline(always)]
    fn from(i: i32) -> Self {
        Val::from_i32(i)
    }
}

impl Val {
    #[inline(always)]
    pub const fn get_payload(&self) -> u64 {
        self.0
    }

    #[inline(always)]
    pub const fn from_payload(&self, x: u64) -> Self {
        Self(x)
    }

    #[inline(always)]
    pub(crate) const fn is_u63(&self) -> bool {
        (self.0 & 1) == 0
    }

    #[inline(always)]
    pub(crate) const unsafe fn unchecked_as_u63(&self) -> i64 {
        (self.0 >> 1) as i64
    }

    #[inline(always)]
    pub(crate) const unsafe fn unchecked_from_u63(i: i64) -> Self {
        Self((i as u64) << 1)
    }

    #[inline(always)]
    pub(crate) fn get_tag(&self) -> Tag {
        unsafe { ::core::mem::transmute(((self.0 >> 1) & TAG_MASK) as u8) }
    }

    #[inline(always)]
    pub(crate) const fn get_body(&self) -> u64 {
        self.0 >> (TAG_BITS + 1)
    }

    #[inline(always)]
    pub(crate) fn has_tag(&self, tag: Tag) -> bool {
        !self.is_u63() && self.get_tag() == tag
    }

    #[inline(always)]
    pub fn is<T: ValType>(self) -> bool {
        T::is_val_type(self)
    }

    #[inline(always)]
    // This does no checking, so it can be used in const fns
    // below; it should not be made public.
    pub(crate) const unsafe fn from_body_and_tag(body: u64, tag: Tag) -> Val {
        let body = (body << TAG_BITS) | (tag as u64);
        Val((body << 1) | 1)
    }

    #[inline(always)]
    // This also does not checking, is a crate-local helper.
    pub(crate) const unsafe fn from_major_minor_and_tag(major: u32, minor: u32, tag: Tag) -> Val {
        let major = major as u64;
        let minor = minor as u64;
        Self::from_body_and_tag((major << MINOR_BITS) | minor, tag)
    }

    #[inline(always)]
    pub(crate) const fn has_minor(&self, minor: u32) -> bool {
        self.get_minor() == minor
    }

    #[inline(always)]
    pub(crate) const fn get_minor(&self) -> u32 {
        (self.get_body() & MINOR_MASK) as u32
    }

    #[inline(always)]
    pub(crate) const fn get_major(&self) -> u32 {
        (self.get_body() >> MINOR_BITS) as u32
    }

    #[inline(always)]
    pub const fn from_void() -> Val {
        unsafe { Val::from_body_and_tag(Static::Void as u64, Tag::Static) }
    }

    #[inline(always)]
    pub const fn from_bool(b: bool) -> Val {
        let body = if b { Static::True } else { Static::False };
        unsafe { Val::from_body_and_tag(body as u64, Tag::Static) }
    }

    #[inline(always)]
    pub const fn from_u32(u: u32) -> Val {
        unsafe { Val::from_body_and_tag(u as u64, Tag::U32) }
    }

    #[inline(always)]
    pub const fn from_i32(i: i32) -> Val {
        unsafe { Val::from_body_and_tag((i as u32) as u64, Tag::I32) }
    }
}
