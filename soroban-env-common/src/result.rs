use crate::{ConversionError, Env, IntoVal, RawVal, Status, TryIntoVal, try_convert_to};

impl<E: Env, T, R> TryIntoVal<E, Result<T, R>> for RawVal
where
    RawVal: TryIntoVal<E, T, Error = ConversionError>,
    R: TryFrom<Status>,
{
    type Error = ConversionError;

    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<Result<T, R>, Self::Error> {
        if let Ok(status) = try_convert_to::<Status,_,_>(self, env) {
            Ok(Err(status.try_into().map_err(|_| ConversionError)?))
        } else {
            Ok(Ok(try_convert_to::<T,_,_>(self, env)?))
        }
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
