use crate::{Env, FromVal, IntoVal, RawVal, TryFromVal, TryIntoVal};

impl<E: Env, T> TryFromVal<E, RawVal> for Option<T>
where
    T: TryFromVal<E, RawVal>,
{
    type Error = <RawVal as TryIntoVal<E, T>>::Error;

    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        if val.is_void() {
            Ok(None)
        } else {
            Ok(Some(T::try_from_val(env, val)?))
        }
    }
}

impl<E: Env, T> TryFromVal<E, Option<T>> for RawVal
where
    T: TryIntoVal<E, RawVal>,
{
    type Error = T::Error;

    fn try_from_val(env: &E, v: Option<T>) -> Result<Self, Self::Error> {
        match v {
            Some(e) => e.try_into_val(env),
            None => Ok(RawVal::VOID),
        }
    }
}

impl<E: Env, T> FromVal<E, RawVal> for Option<T>
where
    T: FromVal<E, RawVal>,
{
    fn from_val(env: &E, v: RawVal) -> Self {
        if v.is_void() {
            None
        } else {
            Some(v.into_val(env))
        }
    }
}

impl<E: Env, T> FromVal<E, Option<T>> for RawVal
where
    T: IntoVal<E, RawVal>,
{
    fn from_val(env: &E, v: Option<T>) -> Self {
        match v {
            Some(t) => t.into_val(env),
            None => RawVal::VOID,
        }
    }
}
