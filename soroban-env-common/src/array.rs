use crate::xdr::ScHostValErrorCode;
use core::borrow::Borrow;
use stellar_xdr::ScObjectType;

use crate::{ConversionError, Env, Object, RawVal, RawValConvertible, Status, TryFromVal};

impl<E: Env, const N: usize> TryFromVal<E, RawVal> for [u8; N] {
    type Error = Status;

    fn try_from_val(env: &E, val: impl Borrow<RawVal>) -> Result<Self, Self::Error> {
        let val = *val.borrow();
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(ScHostValErrorCode::UnexpectedValType.into());
        }
        let bytes = unsafe { Object::unchecked_from_val(val) };
        let len = unsafe { u32::unchecked_from_val(env.bytes_len(bytes)) } as usize;
        if len != N {
            return Err(ConversionError.into());
        }
        let mut arr = [0u8; N];
        env.bytes_copy_to_slice(bytes, RawVal::U32_ZERO, &mut arr)?;
        Ok(arr)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for Vec<u8> {
    type Error = Status;

    fn try_from_val(env: &E, val: impl Borrow<RawVal>) -> Result<Self, Self::Error> {
        let val = *val.borrow();
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(ScHostValErrorCode::UnexpectedValType.into());
        }
        let bytes = unsafe { Object::unchecked_from_val(val) };
        let len = unsafe { u32::unchecked_from_val(env.bytes_len(bytes)) } as usize;
        let mut vec = vec![0u8; len];
        env.bytes_copy_to_slice(bytes, RawVal::U32_ZERO, &mut vec)?;
        Ok(vec)
    }
}

impl<E: Env> TryFromVal<E, [u8]> for RawVal {
    type Error = Status;
    #[inline(always)]
    fn try_from_val(env: &E, v: impl Borrow<[u8]>) -> Result<RawVal, Self::Error> {
        Ok(env.bytes_new_from_slice(v.borrow())?.to_raw())
    }
}

// Technically this impl is redundant but it makes users have to type less
// boilerplate at call sites when converting [u8;N]
impl<E: Env, const N: usize> TryFromVal<E, [u8; N]> for RawVal {
    type Error = Status;

    fn try_from_val(env: &E, v: impl Borrow<[u8; N]>) -> Result<Self, Self::Error> {
        <RawVal as TryFromVal<E, [u8]>>::try_from_val(env, v.borrow().as_slice())
    }
}

// Technically this impl is redundant but it makes users have to type less
// boilerplate at call sites when converting &[u8]
impl<'a, E: Env> TryFromVal<E, &'a [u8]> for RawVal {
    type Error = Status;

    fn try_from_val(env: &E, v: impl Borrow<&'a [u8]>) -> Result<Self, Self::Error> {
        <RawVal as TryFromVal<E, [u8]>>::try_from_val(env, *v.borrow())
    }
}

// Technically this impl is redundant but it makes users have to type less
// boilerplate at call sites when converting Vec<u8>
#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, Vec<u8>> for RawVal {
    type Error = Status;

    fn try_from_val(env: &E, v: impl Borrow<Vec<u8>>) -> Result<Self, Self::Error> {
        <RawVal as TryFromVal<E, [u8]>>::try_from_val(env, v.borrow().as_slice())
    }
}
