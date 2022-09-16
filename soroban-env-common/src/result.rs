use crate::{ConversionError, Env, IntoVal, RawVal, Status, TryFromVal, TryIntoVal};

impl<E: Env, T> TryFromVal<E, RawVal> for Result<T, Status>
where
    T: TryFromVal<E, RawVal, Error = ConversionError>,
{
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        if let Ok(status) = Status::try_from_val(env, val) {
            Ok(Err(status))
        } else {
            Ok(Ok(T::try_from_val(env, val)?))
        }
    }
}

impl<E: Env, T> TryIntoVal<E, Result<T, Status>> for RawVal
where
    T: TryFromVal<E, RawVal, Error = ConversionError>,
{
    type Error = ConversionError;

    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<Result<T, Status>, Self::Error> {
        <_ as TryFromVal<E, RawVal>>::try_from_val(env, self)
    }
}

impl<E: Env, T> IntoVal<E, RawVal> for Result<T, Status>
where
    T: IntoVal<E, RawVal>,
{
    #[inline(always)]
    fn into_val(self, env: &E) -> RawVal {
        match self {
            Ok(t) => t.into_val(env),
            Err(status) => status.into_val(env),
        }
    }
}

impl<E: Env, T, F> IntoVal<E, RawVal> for &Result<T, F>
where
    for<'a> &'a T: IntoVal<E, RawVal>,
    for<'a> &'a F: IntoVal<E, RawVal>,
{
    #[inline(always)]
    fn into_val(self, env: &E) -> RawVal {
        match self {
            Ok(t) => t.into_val(env),
            Err(status) => status.into_val(env),
        }
    }
}
