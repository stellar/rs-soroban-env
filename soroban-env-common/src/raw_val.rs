use stellar_xdr::{ScStatic, ScStatus, ScStatusType};

use super::{
    BitSet, Env, EnvVal, FromVal, IntoVal, Object, Static, Status, Symbol, TryFromVal, TryIntoVal,
};
use core::{convert::Infallible, fmt::Debug};

extern crate static_assertions as sa;

#[allow(dead_code)]
const TAG_MASK: u32 = 0xff_u32;

/// Code values for the 3 "tag" bits in the bit-packed representation
/// of [RawVal].
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Tag {
    // TODO v160: we have enough tag space now to put bool, void,
    // {u,i}{8,16,128}, option and result in the control word.
    /// Tag for a [RawVal] that contains a [u32] number.
    U32 = 0,

    /// Tag for a [RawVal] that contains an [i32] number.
    I32 = 1,

    /// Tag for a [RawVal] that contains a [u64] number.
    U64 = 2,

    /// Tag for a [RawVal] that contains an [i64] number.
    I64 = 3,

    /// Tag for a [RawVal] that contains a "static" value like `true`, `false`, `void`; see [Static](crate::Static).
    Static = 4,

    /// Tag for a [RawVal] that contains a host object handle; see [Object](crate::Object).
    Object = 5,

    /// Tag for a [RawVal] that contains a symbol; see [Symbol](crate::Symbol).
    Symbol = 6,

    /// Tag for a [RawVal] that contains a small bitset; see [BitSet](crate::BitSet).
    BitSet = 7,

    /// Tag for a [RawVal] that contains a status code; see [Status](crate::Status).
    Status = 8,
}

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

/// V2:
///
/// A RawVal is the "large" polymorphic type we're willing to exchange across
/// the Host/Guest barrier, and which is stored in slots inside conainers.
/// It can contain any scalar type, as well as small symbols and other small
/// structured types.
///
/// RawVals are packed 160-bit / 20-byte structs consisting of a 32-bit control
/// value and a 128-bit payload.
///
/// We use the "Basic C ABI" for passing and returning structs, as specified
/// in https://github.com/WebAssembly/tool-conventions/blob/main/BasicCABI.md
///
/// Spelled out: RawVals are passed and returned by reference. References are
/// 32-bit values that point into guest linear memory, usually just the guest's
/// shadow stack (i.e. this does not require the guest to have a heap
/// allocator). To accept a RawVal by reference, the host reads 20 bytes from
/// guest linear memory at the address passed. To return a RawVal by reference,
/// the host writes 20 bytes into the address passed.
///
/// To invoke a guest function with a RawVal argument, the host first increments
/// the guest's stack pointer (which is the first mutable global in the guest)
/// by 20 bytes, and then writes the RawVal into the newly-allocated stack area
/// and passes its address to the guest.
///
/// Host functions do not need to accept _only_ RawVals: they can also accept
/// direct u32 words representing 32-bit Rust numbers, chars or bools. Host
/// functions that _return_ such values can also return them directly, rather
/// than through a reference.

#[repr(C)]
#[derive(Copy, Clone)]
pub struct RawVal {
    pub control: u32,
    pub payload: u128,
}

impl crate::abi::BufReadWrite for RawVal {
    type MemBuf = [u8; 20];
    const ZERO_BUF: Self::MemBuf = [0;20];

    fn buf_write(self, b: &mut Self::MemBuf) {
        let cbuf = u32::to_le_bytes(self.control);
        let pbuf = u128::to_le_bytes(self.payload);
        unsafe {
            let mut bp = b.as_mut_ptr();
            cbuf.as_ptr().copy_to(bp, 4);
            bp = bp.add(4);
            pbuf.as_ptr().copy_to(bp, 16);
        }
    }

    fn buf_read(b: &Self::MemBuf) -> Self {
        let mut cbuf: [u8; 4] = [0; 4];
        let mut pbuf: [u8; 16] = [0; 16];
        unsafe {
            let mut bp = b.as_ptr();
            bp.copy_to(cbuf.as_mut_ptr(), 4);
            bp = bp.add(4);
            bp.copy_to(pbuf.as_mut_ptr(), 16);
        }
        let control = u32::from_le_bytes(cbuf);
        let payload = u128::from_le_bytes(pbuf);
        Self { control, payload }
    }

    fn buf_as_slice(b: &Self::MemBuf) -> &[u8] {
        b.as_slice()
    }

    fn buf_as_mut_slice(b: &mut Self::MemBuf) -> &mut [u8] {
        b.as_mut_slice()
    }
}

impl crate::abi::V160 for RawVal {
    fn v160_explode(self) -> (u32, u64, u64) {
        (
            self.control,
            self.payload as u64,
            (self.payload >> 64) as u64,
        )
    }

    fn v160_implode(a: u32, b: u64, c: u64) -> Self {
        Self {
            control: a,
            payload: b as u128 | ((c as u128) << 64),
        }
    }
}

impl RawVal {
    #[allow(dead_code)]
    pub const fn shallow_eq(&self, other: &RawVal) -> bool {
        self.control == other.control && self.payload == other.payload
    }
}

#[cfg(test)]
#[test]
fn test_rawval_roundtrip() {
    use crate::abi::BufReadWrite;
    let rv = RawVal {
        control: 0x1234_5678,
        payload: 0x1111_2222_3333_4444_5555_6666_7777_8888,
    };
    let mut buf = RawVal::ZERO_BUF;
    rv.buf_write(&mut buf);
    assert_eq!(buf[0], 0x78);
    assert_eq!(buf[1], 0x56);
    assert_eq!(buf[2], 0x34);
    assert_eq!(buf[3], 0x12);
    assert_eq![buf[4], 0x88];
    assert_eq![buf[19], 0x11];
    let bv: RawVal = RawVal::buf_read(&buf);
    assert!(rv.shallow_eq(&bv));
}
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
declare_tryfrom!(u64);
declare_tryfrom!(i64);
declare_tryfrom!(BitSet);
declare_tryfrom!(Status);
declare_tryfrom!(Symbol);
declare_tryfrom!(Static);
declare_tryfrom!(Object);

impl RawValConvertible for () {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::Static) && v.get_lo32() == ScStatic::Void as u32
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(_v: RawVal) -> Self {}
}

impl RawValConvertible for bool {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::Static)
            && (v.get_lo32() == ScStatic::True as u32 || v.get_lo32() == ScStatic::False as u32)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        v.get_lo32() == ScStatic::True as u32
    }
    #[inline(always)]
    fn try_convert(v: RawVal) -> Option<Self> {
        if v.has_tag(Tag::Static) {
            if v.get_lo32() == ScStatic::True as u32 {
                Some(true)
            } else if v.get_lo32() == ScStatic::False as u32 {
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
        v.get_lo32()
    }
}

impl RawValConvertible for i32 {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::I32)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        v.get_lo32() as i32
    }
}

impl RawValConvertible for u64 {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::U64)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        v.get_lo64() as u64
    }
}

impl RawValConvertible for i64 {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::I64)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        v.get_lo64() as i64
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

impl From<u64> for RawVal {
    #[inline(always)]
    fn from(u: u64) -> Self {
        RawVal::from_u64(u)
    }
}

impl From<&u64> for RawVal {
    #[inline(always)]
    fn from(u: &u64) -> Self {
        RawVal::from_u64(*u)
    }
}

impl From<i64> for RawVal {
    #[inline(always)]
    fn from(i: i64) -> Self {
        RawVal::from_i64(i)
    }
}

impl From<&i64> for RawVal {
    #[inline(always)]
    fn from(i: &i64) -> Self {
        RawVal::from_i64(*i)
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
    pub fn in_env<E: Env>(self, env: &E) -> EnvVal<E, RawVal> {
        EnvVal {
            env: env.clone(),
            val: self,
        }
    }

    #[inline(always)]
    const fn get_tag_u8(self) -> u8 {
        (self.control & TAG_MASK) as u8
    }

    #[inline(always)]
    pub const fn get_tag(self) -> Tag {
        unsafe { ::core::mem::transmute(self.get_tag_u8()) }
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
    pub(crate) const unsafe fn from_payload_and_tag(payload: u128, tag: Tag) -> RawVal {
        let control = tag as u32;
        RawVal { control, payload }
    }

    #[inline(always)]
    pub(crate) const unsafe fn from_lo32_and_tag(lo32: u32, tag: Tag) -> RawVal {
        Self::from_payload_and_tag(lo32 as u128, tag)
    }

    #[inline(always)]
    pub(crate) const fn get_lo32(&self) -> u32 {
        self.payload as u32
    }

    #[inline(always)]
    pub(crate) const unsafe fn from_lo64_and_tag(lo64: u64, tag: Tag) -> RawVal {
        Self::from_payload_and_tag(lo64 as u128, tag)
    }

    #[inline(always)]
    pub(crate) const fn get_lo64(&self) -> u64 {
        self.payload as u64
    }

    #[inline(always)]
    pub const fn from_void() -> RawVal {
        unsafe { RawVal::from_lo32_and_tag(ScStatic::Void as u32, Tag::Static) }
    }

    #[inline(always)]
    pub const fn from_bool(b: bool) -> RawVal {
        let body = if b { ScStatic::True } else { ScStatic::False };
        unsafe { RawVal::from_lo32_and_tag(body as u32, Tag::Static) }
    }

    #[inline(always)]
    pub const fn from_other_static(st: ScStatic) -> RawVal {
        unsafe { RawVal::from_lo32_and_tag(st as u32, Tag::Static) }
    }

    #[inline(always)]
    pub const fn from_u32(u: u32) -> RawVal {
        unsafe { RawVal::from_lo32_and_tag(u, Tag::U32) }
    }

    #[inline(always)]
    pub const fn from_i32(i: i32) -> RawVal {
        unsafe { RawVal::from_lo32_and_tag(i as u32, Tag::I32) }
    }

    #[inline(always)]
    pub const fn from_u64(u: u64) -> RawVal {
        unsafe { RawVal::from_lo64_and_tag(u, Tag::U64) }
    }

    #[inline(always)]
    pub const fn from_i64(i: i64) -> RawVal {
        unsafe { RawVal::from_lo64_and_tag(i as u64, Tag::I64) }
    }

    #[inline(always)]
    pub const fn is_i32_zero(self) -> bool {
        self.shallow_eq(&Self::I32_ZERO)
    }

    #[inline(always)]
    pub const fn is_u32_zero(self) -> bool {
        self.shallow_eq(&Self::U32_ZERO)
    }

    #[inline(always)]
    pub const fn is_void(self) -> bool {
        self.shallow_eq(&Self::VOID)
    }

    #[inline(always)]
    pub const fn is_true(self) -> bool {
        self.shallow_eq(&Self::TRUE)
    }

    #[inline(always)]
    pub const fn is_false(self) -> bool {
        self.shallow_eq(&Self::FALSE)
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
        match self.get_tag() {
            Tag::U32 => write!(f, "U32({})", self.get_lo32()),
            Tag::I32 => write!(f, "I32({})", self.get_lo32() as i32),
            Tag::U64 => write!(f, "U64({})", self.get_lo64()),
            Tag::I64 => write!(f, "I64({})", self.get_lo64() as i64),
            Tag::Static => {
                let scs = ScStatic::try_from(self.get_lo32() as i32);
                if let Ok(scs) = scs {
                    write!(f, "{}", scs.name())
                } else {
                    write!(f, "Static({})", self.get_lo32())
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
        }
    }
}

#[test]
#[cfg(feature = "std")]
fn test_debug() {
    use stellar_xdr::ScObjectType;

    use super::{BitSet, Object, Status, Symbol};
    use crate::xdr::{ScHostValErrorCode, ScStatus};
    assert_eq!(format!("{:?}", RawVal::from_void()), "Void");
    assert_eq!(format!("{:?}", RawVal::from_bool(true)), "True");
    assert_eq!(format!("{:?}", RawVal::from_bool(false)), "False");
    assert_eq!(format!("{:?}", RawVal::from_i32(10)), "I32(10)");
    assert_eq!(format!("{:?}", RawVal::from_u32(10)), "U32(10)");
    let bs: BitSet = 0xf7.into();
    assert_eq!(format!("{:?}", bs), "BitSet(0b11110111)");
    assert_eq!(format!("{:?}", Symbol::from_str("hello")), "Symbol(hello)");
    assert_eq!(format!("{:?}", Object::from_type_and_handle(ScObjectType::Vec, 7)), "Object(Vec(#7))");
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
