use stellar_xdr::ScObjectType;

use crate::{ConversionError, Env, EnvVal, IntoVal, Object, RawVal, RawValConvertible, TryIntoVal};

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
        // TODO: Perform copy using linear memory copy.
        for (i, b) in arr.iter_mut().enumerate() {
            let b_val = env.binary_get(bin, (i as u32).into());
            let b_u32 = unsafe { u32::unchecked_from_val(b_val) };
            *b = b_u32 as u8;
        }
        Ok(arr)
    }
}

impl<E: Env, const N: usize> IntoVal<E, RawVal> for [u8; N] {
    fn into_val(self, env: &E) -> RawVal {
        let env = env.clone();
        let mut bin = env.binary_new();
        for b in self {
            let b_u32 = b as u32;
            let b_val = b_u32.into_val(&env);
            bin = env.binary_push(bin, b_val);
        }
        bin.to_raw()
    }
}

impl<E: Env, const N: usize> IntoVal<E, EnvVal<E, RawVal>> for [u8; N] {
    fn into_val(self, env: &E) -> EnvVal<E, RawVal> {
        let rv: RawVal = self.into_val(env);
        EnvVal {
            env: env.clone(),
            val: rv,
        }
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
