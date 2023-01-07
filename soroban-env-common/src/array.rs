use crate::xdr::ScHostValErrorCode;
use stellar_xdr::ScObjectType;

use crate::{
    ConversionError, Env, Object, RawVal, RawValConvertible, Status, TryFromVal, TryIntoVal,
};

// TODO: these conversions happen as RawVal, but they actually take and produce
// Objects; consider making the signatures tighter.

impl<E: Env, const N: usize> TryFromVal<RawVal, E> for [u8; N] {
    type Error = Status;

    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(ScHostValErrorCode::UnexpectedValType.into());
        }
        let env = env.clone();
        let bytes = unsafe { Object::unchecked_from_val(val) };
        let len = unsafe { u32::unchecked_from_val(env.bytes_len(bytes)) };
        if len as usize != N {
            return Err(ConversionError.into());
        }
        let mut arr = [0u8; N];
        env.bytes_copy_to_slice(bytes, RawVal::U32_ZERO, &mut arr)?;
        Ok(arr)
    }
}

impl<E: Env> TryFromVal<&[u8], E> for RawVal {
    type Error = Status;
    fn try_from_val(env: &E, v: &[u8]) -> Result<Self, Self::Error> {
        Ok(env.bytes_new_from_slice(v)?.to_raw())
    }
}

impl<E: Env, const N: usize> TryFromVal<[u8; N], E> for RawVal {
    type Error = Status;

    fn try_from_val(env: &E, v: [u8; N]) -> Result<Self, Self::Error> {
        v.as_slice().try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<Vec<u8>, E> for RawVal {
    type Error = Status;

    fn try_from_val(env: &E, v: Vec<u8>) -> Result<Self, Self::Error> {
        v.as_slice().try_into_val(env)
    }
}
