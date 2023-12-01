use crate::{declare_tag_based_object_wrapper, Env, TryFromVal, Val};

#[cfg(feature = "std")]
use crate::{
    xdr::{ScErrorCode, ScErrorType},
    TryIntoVal,
};

declare_tag_based_object_wrapper!(StringObject);

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, StringObject> for String {
    type Error = crate::Error;

    fn try_from_val(env: &E, v: &StringObject) -> Result<Self, Self::Error> {
        let len: u32 = env.string_len(*v).map_err(Into::into)?.into();
        let len = len as usize;
        let mut vec = std::vec![0; len];
        env.string_copy_to_slice(*v, Val::U32_ZERO, &mut vec)
            .map_err(Into::into)?;
        String::from_utf8(vec).map_err(|_| {
            crate::Error::from_type_and_code(ScErrorType::Value, ScErrorCode::InvalidInput)
        })
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, Val> for String {
    type Error = crate::Error;

    #[inline(always)]
    fn try_from_val(env: &E, val: &Val) -> Result<Self, Self::Error> {
        let so: StringObject = val.try_into()?;
        so.try_into_val(env)
    }
}

impl<E: Env> TryFromVal<E, &str> for StringObject {
    type Error = crate::Error;
    #[inline(always)]
    fn try_from_val(env: &E, val: &&str) -> Result<StringObject, Self::Error> {
        env.string_new_from_slice(val.as_bytes())
            .map_err(Into::into)
    }
}

impl<E: Env> TryFromVal<E, &str> for Val {
    type Error = crate::Error;
    #[inline(always)]
    fn try_from_val(env: &E, val: &&str) -> Result<Val, Self::Error> {
        Ok(StringObject::try_from_val(env, val)?.into())
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, String> for StringObject {
    type Error = crate::Error;

    fn try_from_val(env: &E, v: &String) -> Result<Self, Self::Error> {
        v.as_str().try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, String> for Val {
    type Error = crate::Error;

    fn try_from_val(env: &E, v: &String) -> Result<Self, Self::Error> {
        v.as_str().try_into_val(env)
    }
}
