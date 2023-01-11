use crate::{convert::ConvertObject, ConvertFrom, EnvBase};
use crate::{Object, RawVal, RawValConvertible};
use core::borrow::Borrow;
use stellar_xdr::ScObjectType;

impl<'a, E: EnvBase> ConvertFrom<E, &'a [u8]> for RawVal
where
    E: ConvertObject<&'a [u8]>,
{
    fn convert_from(e: &E, t: impl Borrow<&'a [u8]>) -> Result<Self, E::Error> {
        Ok(e.bytes_new_from_slice(t.borrow())?.to_raw())
    }
}

impl<const N: usize, E: EnvBase> ConvertFrom<E, [u8; N]> for RawVal
where
    E: for<'a> ConvertObject<&'a [u8]>,
{
    fn convert_from(e: &E, t: impl Borrow<[u8; N]>) -> Result<Self, <E as EnvBase>::Error> {
        <RawVal as ConvertFrom<E, &[u8]>>::convert_from(e, t.borrow().as_slice())
    }
}

impl<const N: usize, E: EnvBase> ConvertFrom<E, RawVal> for [u8; N]
where
    E: for<'a> ConvertObject<&'a [u8]>,
{
    fn convert_from(e: &E, t: impl Borrow<RawVal>) -> Result<Self, E::Error> {
        let t = *t.borrow();
        if !Object::val_is_obj_type(t, ScObjectType::Bytes) {
            return Err(e.err_convert_value::<[u8; N]>(t));
        }
        let obj = unsafe { Object::unchecked_from_val(t) };
        let len = e.object_len(obj)?;
        if len != N {
            return Err(e.err_convert_value::<[u8; N]>(t));
        }
        let mut arr = [0u8; N];
        e.bytes_copy_to_slice(obj, RawVal::U32_ZERO, &mut arr)?;
        Ok(arr)
    }
}

#[cfg(feature = "std")]
impl<E: EnvBase> ConvertFrom<E, RawVal> for Vec<u8>
where
    E: for<'a> ConvertObject<&'a [u8]>,
{
    fn convert_from(e: &E, val: impl Borrow<RawVal>) -> Result<Self, E::Error> {
        let val = *val.borrow();
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(e.err_convert_value::<Vec<u8>>(val));
        }
        let bytes = unsafe { Object::unchecked_from_val(val) };
        let len = e.object_len(bytes)?;
        let mut v = vec![0; len];
        e.bytes_copy_to_slice(bytes, RawVal::U32_ZERO, &mut v)?;
        Ok(v)
    }
}
