use crate::{ConversionError, Env, RawVal, TryFromVal};
use core::borrow::Borrow;

#[cfg(feature = "std")]
use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use crate::{Object, RawValConvertible, TryIntoVal};

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for String {
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, val: impl Borrow<RawVal>) -> Result<Self, Self::Error> {
        let val = *val.borrow();
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(ConversionError);
        }
        let obj: Object = val.try_into_val(env)?;
        let len = unsafe { <u32 as RawValConvertible>::unchecked_from_val(env.bytes_len(obj)) };
        let mut vec = std::vec![0; len as usize];
        env.bytes_copy_to_slice(obj, RawVal::U32_ZERO, &mut vec)
            .map_err(|_| ConversionError)?;
        String::from_utf8(vec).map_err(|_| ConversionError)
    }
}

impl<E: Env> TryFromVal<E, str> for RawVal {
    type Error = ConversionError;
    #[inline(always)]
    fn try_from_val(env: &E, val: impl Borrow<str>) -> Result<RawVal, Self::Error> {
        Ok(env
            .bytes_new_from_slice(val.borrow().as_bytes())
            .map_err(|_| ConversionError)?
            .to_raw())
    }
}

// Technically this impl is redundant but it makes users have to type less
// boilerplate at call sites when converting &str
impl<'a, E: Env> TryFromVal<E, &'a str> for RawVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: impl Borrow<&'a str>) -> Result<Self, Self::Error> {
        <RawVal as TryFromVal<E, str>>::try_from_val(env, *v.borrow())
    }
}

// Technically this impl is redundant but it makes users have to type less
// boilerplate at call sites when converting String
#[cfg(feature = "std")]
impl<'a, E: Env> TryFromVal<E, String> for RawVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: impl Borrow<String>) -> Result<Self, Self::Error> {
        <RawVal as TryFromVal<E, str>>::try_from_val(env, v.borrow().as_str())
    }
}
