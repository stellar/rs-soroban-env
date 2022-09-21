use crate::{ConversionError, Env, RawVal, TryIntoVal};

#[cfg(feature = "std")]
use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use crate::{Object, RawValConvertible, TryFromVal};

// TODO: these conversions happen as RawVal, but they actually take and produce
// Objects; consider making the signatures tighter.

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for String {
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        let obj: Object = val.try_into_val(env)?;
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

#[cfg(feature = "std")]
impl<E: Env> TryIntoVal<E, String> for RawVal {
    type Error = ConversionError;

    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<String, Self::Error> {
        <_ as TryFromVal<E, RawVal>>::try_from_val(env, self)
    }
}

impl<E: Env> TryIntoVal<E, RawVal> for &str {
    type Error = ConversionError;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
        Ok(env
            .bytes_new_from_slice(self.as_bytes())
            .map_err(|_| ConversionError)?
            .to_raw())
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryIntoVal<E, RawVal> for String {
    type Error = ConversionError;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
        (&self).try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryIntoVal<E, RawVal> for &String {
    type Error = ConversionError;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
        self.as_str().try_into_val(env)
    }
}
