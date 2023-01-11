use core::borrow::Borrow;
use crate::{RawVal, Convert, ConvertFrom};

#[cfg(feature = "std")]
use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use crate::{Object, RawValConvertible};

impl<'b,'a:'b,C> ConvertFrom<&'b[u8],C,&'a str> for RawVal
where
    C:Convert<&'b [u8]>,
    RawVal: ConvertFrom<&'b [u8], C, &'b [u8]>
{
    fn convert_from(c: &C, t: impl Borrow<&'a str>) -> Result<Self, C::Error> {
        let bytes: &'b [u8] = t.borrow().as_bytes();
        RawVal::convert_from(c, bytes)
    }
}

#[cfg(feature = "std")]
impl<'a, C> ConvertFrom<&'a str, C, RawVal> for String
where C:Convert<&'a str> {
    fn convert_from(c: &C, t: impl Borrow<RawVal>) -> Result<Self, C::Error> {
        let val = *t.borrow();
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(c.val_cvt_err::<String>(val));
        }
        let obj = unsafe { Object::unchecked_from_val(val) };
        let len = c.object_len(obj)?;
        let mut vec = std::vec![0; len];
        c.bytes_copy_to_slice(obj, RawVal::U32_ZERO, &mut vec)?;
        String::from_utf8(vec).map_err(|_| c.val_cvt_err::<String>(val))
    }
}
