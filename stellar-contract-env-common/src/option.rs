use stellar_xdr::ScObjectType;

use crate::{
    raw_val::{RawVal, RawValConvertible},
    Env, EnvVal, IntoEnvVal, IntoVal, Object, TryFromVal,
};

impl<E: Env, T> TryFrom<EnvVal<E, RawVal>> for Option<T>
where
    T: TryFrom<EnvVal<E, RawVal>>,
{
    type Error = ConversionError<EnvVal<E, RawVal>, Option<T>>;

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        if !Object::val_is_obj_type(ev.val, ScObjectType::Vec) {
            return Err(());
        }
        let env = ev.env.clone();
        let vec = unsafe { Object::unchecked_from_val(ev.val) };
        let len: u32 = env.vec_len(vec).try_into()?;
        match len {
            0 => Ok(None),
            1 => Ok({
                let val = env.vec_get(vec, 0u32.into());
                T::try_from_val(&env, val).map_err(|_| ())?
            }),
            _ => Err(ConversionError),
        }
    }
}

impl<E: Env, T> IntoEnvVal<E, RawVal> for Option<T>
where
    T: IntoEnvVal<E, RawVal>,
{
    fn into_env_val(self, env: &E) -> EnvVal<E, RawVal> {
        let env = env.clone();
        let vec = env.vec_new();
        let vec = env.vec_push(vec, self.0.into_val(&env));
        EnvVal {
            env,
            val: vec.to_raw(),
        }
    }
}
