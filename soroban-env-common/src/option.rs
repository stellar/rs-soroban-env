use crate::{Env, EnvVal, IntoVal, RawVal, TryFromVal, TryIntoVal};

impl<E: Env, T> TryFromVal<E, RawVal> for Option<T>
where
    T: TryFromVal<E, RawVal>,
{
    type Error = T::Error;

    fn try_from_val(env: &E, v: RawVal) -> Result<Self, Self::Error> {
        if v.is_void() {
            Ok(None)
        } else {
            Ok(Some(T::try_from_val(env, v)?))
        }
    }
}

impl<E: Env, T> TryIntoVal<E, Option<T>> for RawVal
where
    T: TryFromVal<E, RawVal>,
{
    type Error = T::Error;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<Option<T>, Self::Error> {
        <_ as TryFromVal<E, RawVal>>::try_from_val(env, self)
    }
}

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
