use core::borrow::Borrow;

use crate::{ConversionError, Env, RawVal, EnvConvertObject, CheckedEnv, EnvBase, ConvertFrom, convert::{EnvConvert, EnvConvertError}};

#[cfg(feature = "std")]
use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use crate::{Object, RawValConvertible};

// Dummy EnvConvertObject impl, the ConvertFrom methods here don't use it
// since there are sufficient methods on EnvBase.
impl<'a,E:EnvConvertError> EnvConvertObject<&'a str> for E {}

impl<'a,C> ConvertFrom<&'a str,C> for RawVal
where C:EnvConvert<&'a str,Self>
{
    fn convert_from(t: impl Borrow<&'a str>, c: &C) -> Result<Self, C::Error> {
        let bytes: &[u8] = t.borrow().as_bytes();
        RawVal::convert_from(bytes, c)
    }
}

#[cfg(feature = "std")]
impl<C> ConvertFrom<RawVal,C> for String
where C:EnvConvert<RawVal,Self> {
    fn convert_from(t: impl Borrow<RawVal>, c: &C) -> Result<Self, C::Error> {
        let obj: Object = t.try_into()?;
        if obj.is_obj_type(ScObjectType::Bytes) {
            let len = unsafe { <u32 as RawValConvertible>::unchecked_from_val(c.bytes_len(obj)) };
            let mut vec = std::vec![0; len as usize];
            c.bytes_copy_to_slice(obj, RawVal::U32_ZERO, &mut vec)
                .map_err(|_| ConversionError)?;
            String::from_utf8(vec).map_err(|_| ConversionError)
        } else {
            Err(ConversionError)
        }
    }
}
