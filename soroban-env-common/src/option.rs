use crate::{Env, EnvVal, IntoVal, RawVal};

// impl<E: Env, T: TryFrom<EnvVal<E, RawVal>>> TryFrom<EnvVal<E, RawVal>> for Option<T> {
//     type Error = ConversionError;

//     fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
//         todo!()
//     }
// }

// impl<E: Env, T: TryFrom<EnvVal<E, RawVal>>> TryIntoVal<E, Option<T>> for RawVal {
//     type Error = ConversionError;
//     #[inline(always)]
//     fn try_into_val(self, env: &E) -> Result<Option<T>, Self::Error> {
//         EnvVal {
//             env: env.clone(),
//             val: self,
//         }
//         .try_into()
//     }
// }

impl<E: Env, T> IntoVal<E, RawVal> for &Option<T>
where
    for<'a> &'a T: IntoVal<E, RawVal>,
{
    fn into_val(self, env: &E) -> RawVal {
        match self {
            Some(t) => t.into_val(env),
            None => RawVal::from_void(),
        }
    }
}

impl<E: Env, T> IntoVal<E, EnvVal<E, RawVal>> for &Option<T>
where
    for<'a> &'a Option<T>: IntoVal<E, RawVal>,
{
    fn into_val(self, env: &E) -> EnvVal<E, RawVal> {
        let rv: RawVal = self.into_val(env);
        EnvVal {
            env: env.clone(),
            val: rv,
        }
    }
}

impl<E: Env, T> IntoVal<E, RawVal> for Option<T>
where
    for<'a> &'a Option<T>: IntoVal<E, RawVal>,
{
    fn into_val(self, env: &E) -> RawVal {
        (&self).into_val(env)
    }
}

impl<E: Env, T> IntoVal<E, EnvVal<E, RawVal>> for Option<T>
where
    for<'a> &'a Option<T>: IntoVal<E, EnvVal<E, RawVal>>,
{
    fn into_val(self, env: &E) -> EnvVal<E, RawVal> {
        (&self).into_val(env)
    }
}
