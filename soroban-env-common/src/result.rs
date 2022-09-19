use crate::{ConversionError, Env, IntoVal, RawVal, Status, TryFromVal, TryIntoVal};

impl<E: Env, T, R> TryFromVal<E, RawVal> for Result<T, R>
where
    T: TryFromVal<E, RawVal, Error = ConversionError>,
    R: TryFrom<Status>,
{
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        if let Ok(status) = Status::try_from_val(env, val) {
            Ok(Err(status.try_into().map_err(|_| ConversionError)?))
        } else {
            Ok(Ok(T::try_from_val(env, val)?))
        }
    }
}

impl<E: Env, T, R> TryIntoVal<E, Result<T, R>> for RawVal
where
    T: TryFromVal<E, RawVal, Error = ConversionError>,
    R: TryFrom<Status>,
{
    type Error = ConversionError;

    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<Result<T, R>, Self::Error> {
        <_ as TryFromVal<E, RawVal>>::try_from_val(env, self)
    }
}

impl<E: Env, T, R> IntoVal<E, RawVal> for Result<T, R>
where
    T: IntoVal<E, RawVal>,
    R: Into<Status>,
{
    #[inline(always)]
    fn into_val(self, env: &E) -> RawVal {
        match self {
            Ok(t) => t.into_val(env),
            Err(r) => {
                let status: Status = r.into();
                status.into_val(env)
            }
        }
    }
}

impl<E: Env, T, R> IntoVal<E, RawVal> for &Result<T, R>
where
    for<'a> &'a T: IntoVal<E, RawVal>,
    for<'a> &'a R: Into<Status>,
{
    #[inline(always)]
    fn into_val(self, env: &E) -> RawVal {
        match self {
            Ok(t) => t.into_val(env),
            Err(r) => {
                let status: Status = r.into();
                status.into_val(env)
            }
        }
    }
}
