use crate::{ConversionError, Env, RawVal, TryFromVal};

#[cfg(feature = "std")]
use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use crate::{Object, RawValConvertible, TryIntoVal};

// TODO: these conversions happen as RawVal, but they actually take and produce
// Objects; consider making the signatures tighter.

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for String {
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        let obj: Object = val.try_into()?;
        if obj.is_obj_type(ScObjectType::Bytes) {
            let len = unsafe { <u32 as RawValConvertible>::unchecked_from_val(env.bytes_len(obj)) };
            let mut vec = std::vec![0; len as usize];
            env.bytes_copy_to_slice(obj, RawVal::U32_ZERO, &mut vec)
                .map_err(|_| ConversionError)?;
            String::from_utf8(vec).map_err(|_| ConversionError)
        } else {
            Err(ConversionError)
        }
    }
}

impl<E: Env> TryFromVal<E, &str> for RawVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &str) -> Result<Self, Self::Error> {
        Ok(env
            .bytes_new_from_slice(v.as_bytes())
            .map_err(|_| ConversionError)?
            .to_raw())
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, String> for RawVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: String) -> Result<Self, Self::Error> {
        v.as_str().try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, &String> for RawVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &String) -> Result<Self, Self::Error> {
        v.as_str().try_into_val(env)
    }
}
