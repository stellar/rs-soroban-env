use stellar_xdr::ScObjectType;

use crate::{ConversionError, Env, EnvVal, IntoVal, Object, RawVal, RawValConvertible, TryIntoVal};

const U32_ZERO: RawVal = RawVal::from_u32(0);

impl<E: Env, const N: usize> TryFrom<EnvVal<E, RawVal>> for [u8; N] {
    type Error = ConversionError;

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        if !Object::val_is_obj_type(ev.val, ScObjectType::Bytes) {
            return Err(ConversionError);
        }
        let env = ev.env.clone();
        let bin = unsafe { Object::unchecked_from_val(ev.val) };
        let len = unsafe { u32::unchecked_from_val(env.binary_len(bin)) };
        if len as usize != N {
            return Err(ConversionError);
        }
        let mut arr = [0u8; N];
        env.binary_copy_to_slice(bin, U32_ZERO, &mut arr);
        Ok(arr)
    }
}

impl<E: Env, const N: usize> TryIntoVal<E, [u8; N]> for RawVal {
    type Error = ConversionError;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<[u8; N], Self::Error> {
        EnvVal {
            env: env.clone(),
            val: self,
        }
        .try_into()
    }
}

impl<E: Env> IntoVal<E, RawVal> for &[u8] {
    fn into_val(self, env: &E) -> RawVal {
        let env = env.clone();
        let mut bin = env.binary_new();
        bin = env.binary_copy_from_slice(bin, U32_ZERO, self);
        bin.to_raw()
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
