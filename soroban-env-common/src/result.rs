use crate::{
    convert::{Convert},
    ConvertFrom, RawVal, Status,
};
use core::{borrow::Borrow};

impl<'a, C, T, R> ConvertFrom<T, C, Result<T, R>> for RawVal
where
    C: Convert<T>,
    RawVal: ConvertFrom<T,C,T>,
    Status: From<R>,
    R: Clone,
{
    fn convert_from(
        c: &C,
        t: impl Borrow<Result<T, R>>,
    ) -> Result<Self, C::Error> {
         match t.borrow() {
            Ok(t) => RawVal::convert_from(c, t),
            Err(r) => {
                let status: Status = Status::from(r.clone());
                Err(status.into())
            }
        }
   }
}

impl<C, T, R> ConvertFrom<T, C, RawVal> for Result<T, R>
where
    C: Convert<T>,
    T: ConvertFrom<T, C, RawVal>,
    R: From<Status>,
{
    fn convert_from<>(
        c: &C,
        t: impl Borrow<RawVal>,
    ) -> Result<Self, C::Error> {
        let t = *t.borrow();
        if let Ok(status) = Status::try_from(t) {
            Ok(Err(status.into()))
        } else {
            // Do not collapse this Ok(Ok(x?)) into Ok(x); we want
            // a failure during the inner convert_from to turn into
            // an outermost Err, not an outermost Ok.
            let converted: T = T::convert_from(c, t)?;
            Ok(Ok(converted))
        }
    }
}
