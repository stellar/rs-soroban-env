use crate::{ConversionError, Env, RawVal, Convert};

#[cfg(feature = "std")]
use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use crate::{Object, RawValConvertible};

impl<E:Env> Convert<&str,Object> for E {
    type Error = ConversionError;

    fn convert_ref(&self, f: &&str) -> Result<Object, Self::Error> {
        self.bytes_new_from_slice(f.as_bytes())
            .map_err(|_| ConversionError)
    }
}

#[cfg(feature = "std")]
impl<E:Env> Convert<Object,String> for E {
    type Error = ConversionError;

    fn convert_ref(&self, obj: &Object) -> Result<String, Self::Error> {
        if obj.is_obj_type(ScObjectType::Bytes) {
            let len = unsafe { u32::unchecked_from_val(self.bytes_len(*obj)) };
            let mut vec = std::vec![0; len as usize];
            self.bytes_copy_to_slice(*obj, RawVal::U32_ZERO, &mut vec)
                .map_err(|_| ConversionError)?;
            String::from_utf8(vec).map_err(|_| ConversionError)
        } else {
            Err(ConversionError)
        }
    }
}
