use crate::xdr::ScHostValErrorCode;
use stellar_xdr::ScObjectType;

use crate::{ConversionError, Env, Object, RawVal, RawValConvertible, TryFromVal, TryIntoVal};

impl<E: Env, const N: usize> TryFromVal<E, RawVal> for [u8; N] {
    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, E::Error> {
        let val = *val;
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(env.escalate_status_to_error(ScHostValErrorCode::UnexpectedValType.into()));
        }
        let bytes = unsafe { Object::unchecked_from_val(val) };
        let len = unsafe { u32::unchecked_from_val(env.bytes_len(bytes)?) } as usize;
        if len != N {
            return Err(env.escalate_status_to_error(ConversionError.into()));
        }
        let mut arr = [0u8; N];
        env.bytes_copy_to_slice(bytes, RawVal::U32_ZERO, &mut arr)?;
        Ok(arr)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for Vec<u8> {
    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, E::Error> {
        let val = *val;
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(env.escalate_status_to_error(ScHostValErrorCode::UnexpectedValType.into()));
        }
        let bytes = unsafe { Object::unchecked_from_val(val) };
        let len = unsafe { u32::unchecked_from_val(env.bytes_len(bytes)?) } as usize;
        let mut vec = vec![0u8; len];
        env.bytes_copy_to_slice(bytes, RawVal::U32_ZERO, &mut vec)?;
        Ok(vec)
    }
}

impl<E: Env> TryFromVal<E, &[u8]> for RawVal {
    #[inline(always)]
    fn try_from_val(env: &E, v: &&[u8]) -> Result<RawVal, E::Error> {
        Ok(env.bytes_new_from_slice(v)?.to_raw())
    }
}

impl<E: Env, const N: usize> TryFromVal<E, [u8; N]> for RawVal {
    fn try_from_val(env: &E, v: &[u8; N]) -> Result<Self, E::Error> {
        v.as_slice().try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, Vec<u8>> for RawVal {
    fn try_from_val(env: &E, v: &Vec<u8>) -> Result<Self, E::Error> {
        v.as_slice().try_into_val(env)
    }
}
