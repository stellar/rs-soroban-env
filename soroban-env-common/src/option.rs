use crate::{RawVal, ConvertFrom, convert::Convert};
use core::{borrow::Borrow};


impl<T,C> ConvertFrom<T,C,RawVal> for Option<T>
where
    C: Convert<T>,
    T: ConvertFrom<T,C,RawVal>,
{
    fn convert_from(c: &C, val: impl Borrow<RawVal>) -> Result<Self, C::Error> {
        if val.borrow().is_void() {
            Ok(None)
        } else {
            Ok(Some(T::convert_from(c, val)?))
        }
    }
}

impl<T,C> ConvertFrom<T,C,Option<T>> for RawVal
where
    C: Convert<T>,
    RawVal: ConvertFrom<T,C,T>,
{
    fn convert_from(c: &C, t: impl Borrow<Option<T>>) -> Result<Self, C::Error> {
        match t.borrow() {
            Some(inner) => RawVal::convert_from(c, inner),
            None => Ok(RawVal::VOID),
        }
    }
}
