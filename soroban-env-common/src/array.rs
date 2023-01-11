use core::borrow::Borrow;
use crate::{ConvertFrom, convert::{Convert}};
use stellar_xdr::ScObjectType;
use crate::{Object, RawVal, RawValConvertible};


impl<'a, V, C> ConvertFrom<V, C, &'a [u8]> for RawVal 
where C:Convert<V>
{
    fn convert_from(c: &C, t: impl Borrow<&'a [u8]>) -> Result<Self, C::Error> {
        Ok(c.bytes_new_from_slice(t.borrow())?.to_raw())
    }
}

impl <'a, const N: usize, C> ConvertFrom<&'a [u8], C, RawVal> for [u8;N] 
where C:Convert<&'a [u8]>
{
    fn convert_from(c: &C, t: impl Borrow<RawVal>) -> Result<Self, C::Error> {
        let t = *t.borrow();
        if !Object::val_is_obj_type(t, ScObjectType::Bytes) {
            return Err(c.val_cvt_err::<[u8;N]>(t));
        }
        let obj = unsafe { Object::unchecked_from_val(t) };
        let len = c.object_len(obj)?;
        if len != N {
            return Err(c.val_cvt_err::<[u8;N]>(t));
        }
        let mut arr = [0u8; N];
        c.bytes_copy_to_slice(obj, RawVal::U32_ZERO, &mut arr)?;
        Ok(arr)
     }
}

#[cfg(feature = "std")]
impl<'a, C> ConvertFrom<&'a [u8], C, RawVal> for Vec<u8>
where C:Convert<&'a [u8]>
 {
    fn convert_from(c: &C, val: impl Borrow<RawVal>) -> Result<Self, C::Error> {
         let val = *val.borrow();
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(c.val_cvt_err::<Vec<u8>>(val));
        }
        let bytes = unsafe { Object::unchecked_from_val(val) };
        let len = c.object_len(bytes)?;
        let mut v = vec![0; len];
        c.bytes_copy_to_slice(bytes, RawVal::U32_ZERO, &mut v)?;
        Ok(v)
    }
}
