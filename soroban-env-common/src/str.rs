use crate::{ConversionError, Env, RawVal, TryFromVal};

#[cfg(feature = "std")]
use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use crate::{Object, RawValConvertible, TryIntoVal};

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for String {
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(ConversionError);
        }
        let obj: Object = val.try_into_val(env)?;
        let len = unsafe {
            <u32 as RawValConvertible>::unchecked_from_val(
                env.bytes_len(obj).map_err(|_| ConversionError)?,
            )
        };
        let mut vec = std::vec![0; len as usize];
        env.bytes_copy_to_slice(obj, RawVal::U32_ZERO, &mut vec)
            .map_err(|_| ConversionError)?;
        String::from_utf8(vec).map_err(|_| ConversionError)
    }
}

impl<E: Env> TryFromVal<E, &str> for RawVal {
    type Error = ConversionError;
    #[inline(always)]
    fn try_from_val(env: &E, val: &&str) -> Result<RawVal, Self::Error> {
        Ok(env
            .bytes_new_from_slice(val.as_bytes())
            .map_err(|_| ConversionError)?
            .to_raw())
    }
}

#[cfg(feature = "std")]
impl<'a, E: Env> TryFromVal<E, String> for RawVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &String) -> Result<Self, Self::Error> {
        v.as_str().try_into_val(env)
    }
}
