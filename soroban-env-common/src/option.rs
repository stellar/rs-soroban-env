use crate::{Env, IntoVal, RawVal, TryIntoVal};

impl<E: Env, T> TryIntoVal<E, Option<T>> for RawVal
where
    RawVal: TryIntoVal<E, T>,
{
    type Error = <RawVal as TryIntoVal<E, T>>::Error;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<Option<T>, Self::Error> {
        if self.is_void() {
            Ok(None)
        } else {
            Ok(Some(<Self as TryIntoVal<E, T>>::try_into_val(self, env)?))
        }
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

impl<E: Env, T> IntoVal<E, RawVal> for Option<T>
where
    T: IntoVal<E, RawVal>,
{
    fn into_val(self, env: &E) -> RawVal {
        match self {
            Some(t) => t.into_val(env),
            None => RawVal::from_void(),
        }
    }
}
