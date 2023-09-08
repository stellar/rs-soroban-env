use crate::{Env, Error, TryFromVal, TryIntoVal, Val};

impl<E: Env, T, R> TryFromVal<E, Val> for Result<T, R>
where
    T: TryFromVal<E, Val>,
    R: TryFrom<Error>,
    <R as TryFrom<Error>>::Error: Into<Error>,
{
    type Error = crate::Error;

    #[inline(always)]
    fn try_from_val(env: &E, val: &Val) -> Result<Self, Self::Error> {
        let val = *val;
        if let Ok(error) = Error::try_from_val(env, &val) {
            match R::try_from(error) {
                Ok(err) => Ok(Err(err)),
                Err(err) => Err(err.into()),
            }
        } else {
            let converted = T::try_from_val(env, &val).map_err(Into::into)?;
            Ok(Ok(converted))
        }
    }
}

impl<E: Env, T, R> TryFromVal<E, Result<T, R>> for Val
where
    Val: TryFromVal<E, T>,
    for<'a> &'a R: TryInto<Error>,
    for<'a> <&'a R as TryInto<Error>>::Error: Into<Error>,
{
    type Error = crate::Error;

    #[inline(always)]
    fn try_from_val(env: &E, v: &Result<T, R>) -> Result<Self, Self::Error> {
        match v {
            Ok(t) => t.try_into_val(env).map_err(Into::into),
            Err(r) => {
                let error: Error = r.try_into().map_err(Into::into)?;
                Ok(error.into())
            }
        }
    }
}
