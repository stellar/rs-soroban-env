use crate::{ConversionError, Env, RawVal, Status, TryFromVal, TryIntoVal};
use core::borrow::Borrow;

impl<E: Env, T, R> TryFromVal<E, RawVal> for Result<T, R>
where
    T: TryFromVal<E, RawVal>,
    R: TryFrom<Status>,
{
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, val: impl Borrow<RawVal>) -> Result<Self, Self::Error> {
        let val = *val.borrow();
        if let Ok(status) = Status::try_from_val(env, val) {
            Ok(Err(status.try_into().map_err(|_| ConversionError)?))
        } else {
            let converted = T::try_from_val(env, val).map_err(|_| ConversionError)?;
            Ok(Ok(converted))
        }
    }
}

impl<E: Env, T, R> TryFromVal<E, Result<T, R>> for RawVal
where
    RawVal: TryFromVal<E, T>,
    Status: for<'a> TryFrom<&'a R>,
{
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, v: impl Borrow<Result<T, R>>) -> Result<Self, Self::Error> {
        match v.borrow() {
            Ok(t) => t.try_into_val(env).map_err(|_| ConversionError),
            Err(r) => {
                let status: Status = Status::try_from(r).map_err(|_| ConversionError)?;
                Ok(status.into())
            }
        }
    }
}
