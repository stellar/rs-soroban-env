use crate::{Env, RawVal, TryFromVal, TryIntoVal};

impl<E: Env, T> TryFromVal<E, RawVal> for Option<T>
where
    T: TryFromVal<E, RawVal>,
{
    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, E::Error> {
        let val = *val;
        if val.is_void() {
            Ok(None)
        } else {
            Ok(Some(T::try_from_val(env, &val)?))
        }
    }
}

impl<E: Env, T> TryFromVal<E, Option<T>> for RawVal
where
    T: TryIntoVal<E, RawVal>,
{
    fn try_from_val(env: &E, v: &Option<T>) -> Result<Self, E::Error> {
        match v {
            Some(t) => t.try_into_val(env),
            None => Ok(RawVal::from_void()),
        }
    }
}
