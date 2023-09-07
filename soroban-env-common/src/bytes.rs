use crate::{
    declare_tag_based_object_wrapper,
    xdr::{ScErrorCode, ScErrorType},
    Env, Error, TryFromVal, TryIntoVal, Val,
};

declare_tag_based_object_wrapper!(BytesObject);

impl<E: Env, const N: usize> TryFromVal<E, BytesObject> for [u8; N] {
    type Error = Error;

    fn try_from_val(env: &E, val: &BytesObject) -> Result<Self, Self::Error> {
        let len: u32 = env.bytes_len(*val).map_err(Into::into)?.into();
        let len = len as usize;
        if len != N {
            return Err(Error::from_type_and_code(
                ScErrorType::Value,
                ScErrorCode::UnexpectedSize,
            ));
        }
        let mut arr = [0u8; N];
        env.bytes_copy_to_slice(*val, Val::U32_ZERO, &mut arr)
            .map_err(Into::into)?;
        Ok(arr)
    }
}

impl<E: Env, const N: usize> TryFromVal<E, Val> for [u8; N] {
    type Error = Error;

    fn try_from_val(env: &E, val: &Val) -> Result<Self, Self::Error> {
        let bo: BytesObject = val.try_into()?;
        bo.try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, BytesObject> for Vec<u8> {
    type Error = Error;

    fn try_from_val(env: &E, val: &BytesObject) -> Result<Self, Self::Error> {
        let len: u32 = env.bytes_len(*val).map_err(Into::into)?.into();
        let len = len as usize;
        let mut vec = vec![0u8; len];
        env.bytes_copy_to_slice(*val, Val::U32_ZERO, &mut vec)
            .map_err(Into::into)?;
        Ok(vec)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, Val> for Vec<u8> {
    type Error = Error;

    fn try_from_val(env: &E, val: &Val) -> Result<Self, Self::Error> {
        let bo: BytesObject = val.try_into()?;
        bo.try_into_val(env)
    }
}

impl<E: Env> TryFromVal<E, &[u8]> for BytesObject {
    type Error = Error;
    #[inline(always)]
    fn try_from_val(env: &E, v: &&[u8]) -> Result<BytesObject, Self::Error> {
        env.bytes_new_from_slice(v).map_err(Into::into)
    }
}

impl<E: Env> TryFromVal<E, &[u8]> for Val {
    type Error = Error;
    #[inline(always)]
    fn try_from_val(env: &E, v: &&[u8]) -> Result<Val, Self::Error> {
        Ok(BytesObject::try_from_val(env, v)?.into())
    }
}

impl<E: Env, const N: usize> TryFromVal<E, [u8; N]> for BytesObject {
    type Error = Error;

    fn try_from_val(env: &E, v: &[u8; N]) -> Result<Self, Self::Error> {
        v.as_slice().try_into_val(env)
    }
}

impl<E: Env, const N: usize> TryFromVal<E, [u8; N]> for Val {
    type Error = Error;

    fn try_from_val(env: &E, v: &[u8; N]) -> Result<Self, Self::Error> {
        v.as_slice().try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, Vec<u8>> for BytesObject {
    type Error = Error;

    fn try_from_val(env: &E, v: &Vec<u8>) -> Result<Self, Self::Error> {
        v.as_slice().try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, Vec<u8>> for Val {
    type Error = Error;

    fn try_from_val(env: &E, v: &Vec<u8>) -> Result<Self, Self::Error> {
        v.as_slice().try_into_val(env)
    }
}
