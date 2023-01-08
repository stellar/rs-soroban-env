use core::borrow::Borrow;

use crate::{ConvertFrom, convert::{EnvConvert, EnvConvertError}, EnvConvertObject};
use stellar_xdr::ScObjectType;

use crate::{
    ConversionError, Object, RawVal, RawValConvertible
};

// Dummy EnvConvertObject impl, the ConvertFrom methods here don't use it
// since there are sufficient methods on EnvBase.
impl<'a, E: EnvConvertError> EnvConvertObject<&'a [u8]> for E {}

impl<'a> ConvertFrom<&'a [u8]> for RawVal {
    fn convert_from<C:EnvConvert<&'a [u8],Self>>(t: impl Borrow<&'a [u8]>, c: &C) -> Result<Self, C::Error> {
        Ok(c.bytes_new_from_slice(t.borrow())?.to_raw())
    }
}

impl <const N: usize> ConvertFrom<RawVal> for [u8;N] {
    fn convert_from<C:EnvConvert<RawVal,Self>>(t: impl Borrow<RawVal>, c: &C) -> Result<Self, C::Error> {
        if !Object::val_is_obj_type(t, ScObjectType::Bytes) {
            return Err(c.cvt_err::<RawVal,[u8;N]>(t));
        }
        let obj = unsafe { Object::unchecked_from_val(t) };
        let len = unsafe { u32::unchecked_from_val(c.bytes_len(obj)) };
        if len as usize != N {
            return Err(ConversionError.into());
        }
        let mut arr = [0u8; N];
        c.bytes_copy_to_slice(obj, RawVal::U32_ZERO, &mut arr)?;
        Ok(arr)
    }
}


#[cfg(feature = "std")]
impl ConvertFrom<RawVal> for Vec<u8> {
    fn convert_from<C:EnvConvert<RawVal,Self>>(val: impl Borrow<RawVal>, c: &C) -> Result<Self, C::Error> {
        if !Object::val_is_obj_type(val, ScObjectType::Bytes) {
            return Err(c.cvt_err::<RawVal,Vec<u8>>(val));
        }
        let bytes = unsafe { Object::unchecked_from_val(val) };
        let len = unsafe { u32::unchecked_from_val(c.bytes_len(bytes)) };
        let mut v = vec![0; len];
        c.bytes_copy_to_slice(bytes, RawVal::U32_ZERO, &mut v)?;
        Ok(v)
        
    }
}
