use crate::{ConversionError, Env, Error, TryFromVal, TryIntoVal, Val};

impl<E: Env, T, R> TryFromVal<E, Val> for Result<T, R>
where
    T: TryFromVal<E, Val>,
    R: TryFrom<Error>,
{
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, val: &Val) -> Result<Self, Self::Error> {
        let val = *val;
        if let Ok(status) = Error::try_from_val(env, &val) {
            Ok(Err(status.try_into().map_err(|_| ConversionError)?))
        } else {
            let converted = T::try_from_val(env, &val).map_err(|_| ConversionError)?;
            Ok(Ok(converted))
        }
    }
}

impl<E: Env, T, R> TryFromVal<E, Result<T, R>> for Val
where
    Val: TryFromVal<E, T>,
    Error: for<'a> TryFrom<&'a R>,
{
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, v: &Result<T, R>) -> Result<Self, Self::Error> {
        match v {
            Ok(t) => t.try_into_val(env).map_err(|_| ConversionError),
            Err(r) => {
                let status: Error = Error::try_from(r).map_err(|_| ConversionError)?;
                Ok(status.into())
            }
        }
    }
}
