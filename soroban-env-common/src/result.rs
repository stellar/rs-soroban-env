use crate::{ConversionError, Env, MapErrToEnv, RawVal, Status, TryFromVal, TryIntoVal};

impl<E: Env, T, R> TryFromVal<E, RawVal> for Result<T, R>
where
    T: TryFromVal<E, RawVal>,
    R: TryFrom<Status>,
{
    #[inline(always)]
    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, E::Error> {
        let val = *val;
        if let Ok(status) = Status::try_from_val(env, &val) {
            Ok(Err(status
                .try_into()
                .map_err(|_| ConversionError)
                .map_err_to_env(env)?))
        } else {
            let converted = T::try_from_val(env, &val)?;
            Ok(Ok(converted))
        }
    }
}

impl<E: Env, T, R> TryFromVal<E, Result<T, R>> for RawVal
where
    RawVal: TryFromVal<E, T>,
    Status: for<'a> TryFrom<&'a R>,
{
    #[inline(always)]
    fn try_from_val(env: &E, v: &Result<T, R>) -> Result<Self, E::Error> {
        match v {
            Ok(t) => t.try_into_val(env),
            Err(r) => {
                let status: Status = Status::try_from(r)
                    .map_err(|_| ConversionError)
                    .map_err_to_env(env)?;
                Ok(status.into())
            }
        }
    }
}
