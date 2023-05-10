use crate::{
    declare_tag_based_object_wrapper, ConversionError, Env, Error, RawVal, TryFromVal, TryIntoVal,
};

declare_tag_based_object_wrapper!(BytesObject);

impl<E: Env, const N: usize> TryFromVal<E, BytesObject> for [u8; N] {
    type Error = Error;

    fn try_from_val(env: &E, val: &BytesObject) -> Result<Self, Self::Error> {
        let len: u32 = env.bytes_len(*val).map_err(|_| ConversionError)?.into();
        let len = len as usize;
        if len != N {
            return Err(ConversionError.into());
        }
        let mut arr = [0u8; N];
        env.bytes_copy_to_slice(*val, RawVal::U32_ZERO, &mut arr)
            .map_err(|_| ConversionError)?;
        Ok(arr)
    }
}

impl<E: Env, const N: usize> TryFromVal<E, RawVal> for [u8; N] {
    type Error = Error;

    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, Self::Error> {
        let bo: BytesObject = val.try_into()?;
        bo.try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, BytesObject> for Vec<u8> {
    type Error = Error;

    fn try_from_val(env: &E, val: &BytesObject) -> Result<Self, Self::Error> {
        let len: u32 = env.bytes_len(*val).map_err(|_| ConversionError)?.into();
        let len = len as usize;
        let mut vec = vec![0u8; len];
        env.bytes_copy_to_slice(*val, RawVal::U32_ZERO, &mut vec)
            .map_err(|_| ConversionError)?;
        Ok(vec)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for Vec<u8> {
    type Error = Error;

    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, Self::Error> {
        let bo: BytesObject = val.try_into()?;
        bo.try_into_val(env)
    }
}

impl<E: Env> TryFromVal<E, &[u8]> for BytesObject {
    type Error = Error;
    #[inline(always)]
    fn try_from_val(env: &E, v: &&[u8]) -> Result<BytesObject, Self::Error> {
        Ok(env.bytes_new_from_slice(v).map_err(|_| ConversionError)?)
    }
}

impl<E: Env> TryFromVal<E, &[u8]> for RawVal {
    type Error = Error;
    #[inline(always)]
    fn try_from_val(env: &E, v: &&[u8]) -> Result<RawVal, Self::Error> {
        Ok(BytesObject::try_from_val(env, v)?.into())
    }
}

impl<E: Env, const N: usize> TryFromVal<E, [u8; N]> for BytesObject {
    type Error = Error;

    fn try_from_val(env: &E, v: &[u8; N]) -> Result<Self, Self::Error> {
        v.as_slice().try_into_val(env)
    }
}

impl<E: Env, const N: usize> TryFromVal<E, [u8; N]> for RawVal {
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
impl<E: Env> TryFromVal<E, Vec<u8>> for RawVal {
    type Error = Error;

    fn try_from_val(env: &E, v: &Vec<u8>) -> Result<Self, Self::Error> {
        v.as_slice().try_into_val(env)
    }
}
