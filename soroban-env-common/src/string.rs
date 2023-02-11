use crate::{declare_tag_based_object_wrapper, ConversionError, Env, RawVal, TryFromVal};

#[cfg(feature = "std")]
use crate::TryIntoVal;

declare_tag_based_object_wrapper!(StringObject);

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, StringObject> for String {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &StringObject) -> Result<Self, Self::Error> {
        let len: u32 = env.string_len(*v).map_err(|_| ConversionError)?.into();
        let len = len as usize;
        let mut vec = std::vec![0; len as usize];
        env.string_copy_to_slice(*v, RawVal::U32_ZERO, &mut vec)
            .map_err(|_| ConversionError)?;
        String::from_utf8(vec).map_err(|_| ConversionError)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for String {
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, Self::Error> {
        let so: StringObject = val.try_into()?;
        so.try_into_val(env)
    }
}

impl<E: Env> TryFromVal<E, &str> for StringObject {
    type Error = ConversionError;
    #[inline(always)]
    fn try_from_val(env: &E, val: &&str) -> Result<StringObject, Self::Error> {
        Ok(env
            .string_new_from_slice(*val)
            .map_err(|_| ConversionError)?)
    }
}

impl<E: Env> TryFromVal<E, &str> for RawVal {
    type Error = ConversionError;
    #[inline(always)]
    fn try_from_val(env: &E, val: &&str) -> Result<RawVal, Self::Error> {
        Ok(StringObject::try_from_val(env, val)?.into())
    }
}

#[cfg(feature = "std")]
impl<'a, E: Env> TryFromVal<E, String> for StringObject {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &String) -> Result<Self, Self::Error> {
        v.as_str().try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<'a, E: Env> TryFromVal<E, String> for RawVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &String) -> Result<Self, Self::Error> {
        v.as_str().try_into_val(env)
    }
}
