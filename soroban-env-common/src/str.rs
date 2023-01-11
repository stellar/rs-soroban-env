use crate::{ConvertFrom, ConvertObject, RawVal};
use core::borrow::Borrow;

#[cfg(feature = "std")]
use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use crate::{Object, RawValConvertible};

impl<'b, 'a: 'b, E> ConvertFrom<E, &'a str> for RawVal
where
    E: ConvertObject<&'b [u8]>,
    RawVal: ConvertFrom<E, &'b [u8]>,
{
    fn convert_from(e: &E, t: impl Borrow<&'a str>) -> Result<Self, E::Error> {
        let bytes: &'b [u8] = t.borrow().as_bytes();
        RawVal::convert_from(e, bytes)
    }
}

#[cfg(feature = "std")]
impl<'a, E> ConvertFrom<E, RawVal> for String
where
    E: ConvertObject<&'a str>,
{
    fn convert_from(e: &E, t: impl Borrow<RawVal>) -> Result<Self, E::Error> {
        let val = *t.borrow();
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(e.err_convert_value::<String>(val));
        }
        let obj = unsafe { Object::unchecked_from_val(val) };
        let len = e.object_len(obj)?;
        let mut vec = std::vec![0; len];
        e.bytes_copy_to_slice(obj, RawVal::U32_ZERO, &mut vec)?;
        String::from_utf8(vec).map_err(|_| e.err_convert_value::<String>(val))
    }
}
