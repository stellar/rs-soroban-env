use crate::{convert::ConvertObject, ConvertFrom, RawVal, Status};
use core::borrow::Borrow;

impl<T, E, R> ConvertFrom<E, Result<T, R>> for RawVal
where
    E: ConvertObject<T>,
    RawVal: ConvertFrom<E, T>,
    Status: for<'a> From<&'a R>,
{
    fn convert_from(e: &E, t: impl Borrow<Result<T, R>>) -> Result<Self, E::Error> {
        match t.borrow() {
            Ok(t) => RawVal::convert_from(e, t),
            Err(r) => {
                let status: Status = Status::from(r);
                Err(status.into())
            }
        }
    }
}

impl<E, T, R> ConvertFrom<E, RawVal> for Result<T, R>
where
    E: ConvertObject<T>,
    T: ConvertFrom<E, RawVal>,
    R: From<Status>,
{
    fn convert_from(e: &E, t: impl Borrow<RawVal>) -> Result<Self, E::Error> {
        let t = *t.borrow();
        if let Ok(status) = Status::try_from(t) {
            Ok(Err(status.into()))
        } else {
            // Do not collapse this Ok(Ok(x?)) into Ok(x); we want
            // a failure during the inner convert_from to turn into
            // an outermost Err, not an outermost Ok.
            let converted: T = T::convert_from(e, t)?;
            Ok(Ok(converted))
        }
    }
}
