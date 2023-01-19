use crate::{Env, RawVal, TryFromVal};

#[cfg(feature = "std")]
use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use crate::{ConversionError, MapErrToEnv, Object, RawValConvertible, TryIntoVal};

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for String {
    #[inline(always)]
    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, E::Error> {
        let val = *val;
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(ConversionError).map_err_to_env(env);
        }
        let obj: Object = val.try_into_val(env)?;
        let len = unsafe { <u32 as RawValConvertible>::unchecked_from_val(env.bytes_len(obj)?) };
        let mut vec = std::vec![0; len as usize];
        env.bytes_copy_to_slice(obj, RawVal::U32_ZERO, &mut vec)?;
        String::from_utf8(vec)
            .map_err(|_| ConversionError)
            .map_err_to_env(env)
    }
}

impl<E: Env> TryFromVal<E, &str> for RawVal {
    #[inline(always)]
    fn try_from_val(env: &E, val: &&str) -> Result<RawVal, E::Error> {
        Ok(env.bytes_new_from_slice(val.as_bytes())?.to_raw())
    }
}

#[cfg(feature = "std")]
impl<'a, E: Env> TryFromVal<E, String> for RawVal {
    fn try_from_val(env: &E, v: &String) -> Result<Self, E::Error> {
        v.as_str().try_into_val(env)
    }
}
