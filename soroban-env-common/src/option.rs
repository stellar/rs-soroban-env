use crate::{ConvertFrom, ConvertObject, EnvBase, RawVal};
use core::borrow::Borrow;

impl<T, E: EnvBase> ConvertFrom<E, RawVal> for Option<T>
where
    E: ConvertObject<T>,
    T: ConvertFrom<E, RawVal>,
{
    fn convert_from(e: &E, val: impl Borrow<RawVal>) -> Result<Self, E::Error> {
        if val.borrow().is_void() {
            Ok(None)
        } else {
            Ok(Some(T::convert_from(e, val)?))
        }
    }
}

impl<T, E: EnvBase> ConvertFrom<E, Option<T>> for RawVal
where
    E: ConvertObject<T>,
    RawVal: ConvertFrom<E, T>,
{
    fn convert_from(e: &E, t: impl Borrow<Option<T>>) -> Result<Self, E::Error> {
        match t.borrow() {
            Some(inner) => RawVal::convert_from(e, inner),
            None => Ok(RawVal::VOID),
        }
    }
}
