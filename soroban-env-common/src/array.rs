use stellar_xdr::ScObjectType;

use crate::{
    ConversionError, Env, EnvVal, IntoVal, Object, RawVal, RawValConvertible, TryFromVal,
    TryIntoVal,
};

impl<E: Env, const N: usize> TryFromVal<E, RawVal> for [u8; N] {
    type Error = ConversionError;

    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(ConversionError);
        }
        let env = env.clone();
        let bytes = unsafe { Object::unchecked_from_val(val) };
        let len = unsafe { u32::unchecked_from_val(env.bytes_len(bytes)) };
        if len as usize != N {
            return Err(ConversionError);
        }
        let mut arr = [0u8; N];
        env.bytes_copy_to_slice(bytes, RawVal::U32_ZERO, &mut arr);
        Ok(arr)
    }
}

impl<E: Env, const N: usize> TryIntoVal<E, [u8; N]> for RawVal {
    type Error = <[u8; N] as TryFromVal<E, RawVal>>::Error;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<[u8; N], Self::Error> {
        <_ as TryFromVal<_, _>>::try_from_val(env, self)
    }
}

impl<E: Env> IntoVal<E, RawVal> for &[u8] {
    fn into_val(self, env: &E) -> RawVal {
        let env = env.clone();
        let mut bytes = env.bytes_new();
        bytes = env.bytes_copy_from_slice(bytes, RawVal::U32_ZERO, self);
        bytes.to_raw()
    }
}

impl<E: Env> IntoVal<E, EnvVal<E, RawVal>> for &[u8] {
    fn into_val(self, env: &E) -> EnvVal<E, RawVal> {
        let rv: RawVal = self.into_val(env);
        EnvVal {
            env: env.clone(),
            val: rv,
        }
    }
}

impl<E: Env, const N: usize> IntoVal<E, RawVal> for &[u8; N] {
    fn into_val(self, env: &E) -> RawVal {
        let slice: &[u8] = self;
        slice.into_val(env)
    }
}

impl<E: Env, const N: usize> IntoVal<E, EnvVal<E, RawVal>> for &[u8; N] {
    fn into_val(self, env: &E) -> EnvVal<E, RawVal> {
        let slice: &[u8] = self;
        slice.into_val(env)
    }
}

impl<E: Env, const N: usize> IntoVal<E, RawVal> for [u8; N] {
    fn into_val(self, env: &E) -> RawVal {
        (&self).into_val(env)
    }
}

impl<E: Env, const N: usize> IntoVal<E, EnvVal<E, RawVal>> for [u8; N] {
    fn into_val(self, env: &E) -> EnvVal<E, RawVal> {
        (&self).into_val(env)
    }
}
