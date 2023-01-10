use crate::{RawVal, ConvertFrom, convert::EnvConvert};
use core::{fmt::Debug, borrow::Borrow};

/* impl<T> ConvertFrom<RawVal> for Option<T>
where T: ConvertFrom<RawVal>
{
    fn convert_from<C:EnvConvert<RawVal,Self>>(val: impl Borrow<RawVal>, c: &C) -> Result<Self, C::Error> {
        if val.is_void() {
            Ok(None)
        } else {
            Ok(Some(T::convert_from(val, c)?))
        }
    }
}
impl<C, T> ConvertFrom<Option<T>> for RawVal
where
    RawVal: ConvertFrom<T>,
{
    fn convert_from<C:EnvConvert<Option<T>,Self>>(t: impl Borrow<Option<T>>, c: &C) -> Result<Self, C::Error> {
        match t.borrow() {
            Some(e) => RawVal::convert_from(e, c),
            None => Ok(RawVal::VOID),
        }
    }
}

 */
