use crate::{Env, TryFromVal, TryIntoVal, Val};

impl<E: Env, T> TryFromVal<E, Val> for Option<T>
where
    T: TryFromVal<E, Val>,
{
    type Error = T::Error;

    fn try_from_val(env: &E, val: &Val) -> Result<Self, Self::Error> {
        if val.is_void() {
            Ok(None)
        } else {
            Ok(Some(T::try_from_val(env, val)?))
        }
    }
}

impl<E: Env, T> TryFromVal<E, Option<T>> for Val
where
    T: TryIntoVal<E, Val>,
{
    type Error = T::Error;

    fn try_from_val(env: &E, v: &Option<T>) -> Result<Self, Self::Error> {
        match v {
            Some(t) => t.try_into_val(env),
            None => Ok(Val::VOID.into()),
        }
    }
}
