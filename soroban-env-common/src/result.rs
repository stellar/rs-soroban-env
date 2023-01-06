use crate::{ConversionError, Env, FromVal, IntoVal, RawVal, Status, TryFromVal};

impl<E: Env, T, R> TryFromVal<E, RawVal> for Result<T, R>
where
    T: TryFromVal<E, RawVal, Error = ConversionError>,
    R: TryFrom<Status>,
{
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        if let Ok(status) = Status::try_from(val) {
            Ok(Err(status.try_into().map_err(|_| ConversionError)?))
        } else {
            Ok(Ok(T::try_from_val(env, val)?))
        }
    }
}

impl<E: Env, T, R> FromVal<E, Result<T, R>> for RawVal
where
    T: IntoVal<E, RawVal>,
    R: Into<Status>,
{
    fn from_val(env: &E, v: Result<T, R>) -> Self {
        match v {
            Ok(t) => t.into_val(env),
            Err(r) => {
                let status: Status = r.into();
                status.into()
            }
        }
    }
}

impl<E: Env, T, R> FromVal<E, &Result<T, R>> for RawVal
where
    for<'a> &'a T: IntoVal<E, RawVal>,
    for<'a> &'a R: Into<Status>,
{
    fn from_val(env: &E, v: &Result<T, R>) -> Self {
        match v {
            Ok(t) => t.into_val(env),
            Err(r) => {
                let status: Status = r.into();
                status.into()
            }
        }
    }
}
