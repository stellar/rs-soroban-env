use crate::xdr::ScHostValErrorCode;
use stellar_xdr::ScObjectType;

use crate::{ConversionError, Env, Object, RawVal, RawValConvertible, Status, TryIntoVal};

// TODO: these conversions happen as RawVal, but they actually take and produce
// Objects; consider making the signatures tighter.

impl<E: Env, const N: usize> TryIntoVal<E, [u8; N]> for RawVal {
    type Error = Status;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<[u8; N], Self::Error> {
        if !Object::val_is_obj_type(self, ScObjectType::Bytes) {
            return Err(ScHostValErrorCode::UnexpectedValType.into());
        }
        let env = env.clone();
        let bytes = unsafe { Object::unchecked_from_val(self) };
        let len = unsafe { u32::unchecked_from_val(env.bytes_len(bytes)) };
        if len as usize != N {
            return Err(ConversionError.into());
        }
        let mut arr = [0u8; N];
        env.bytes_copy_to_slice(bytes, RawVal::U32_ZERO, &mut arr)?;
        Ok(arr)
    }
}

impl<E: Env> TryIntoVal<E, RawVal> for &[u8] {
    type Error = Status;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
        Ok(env.bytes_new_from_slice(self)?.to_raw())
    }
}

impl<E: Env, const N: usize> TryIntoVal<E, RawVal> for [u8; N] {
    type Error = Status;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
        self.as_slice().try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryIntoVal<E, RawVal> for Vec<u8> {
    type Error = Status;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
        (&self).try_into_val(env)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryIntoVal<E, RawVal> for &Vec<u8> {
    type Error = Status;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
        self.as_slice().try_into_val(env)
    }
}
