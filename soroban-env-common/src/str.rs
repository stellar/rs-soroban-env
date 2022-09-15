#![cfg(feature = "std")]

use stellar_xdr::ScObjectType;

use crate::{
    ConversionError, Env, IntoVal, Object, RawVal, RawValConvertible, TryFromVal, TryIntoVal,
};

impl<E: Env> TryFromVal<E, RawVal> for String {
    type Error = ConversionError;

    #[inline(always)]
    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        let obj: Object = val.try_into_val(env)?;
        if obj.is_obj_type(ScObjectType::Bytes) {
            let len = unsafe { <u32 as RawValConvertible>::unchecked_from_val(env.bytes_len(obj)) };
            let mut vec = std::vec![0; len as usize];
            env.bytes_copy_to_slice(obj, RawVal::U32_ZERO, &mut vec);
            String::from_utf8(vec).map_err(|_| ConversionError)
        } else {
            Err(ConversionError)
        }
    }
}

impl<E: Env> TryIntoVal<E, String> for RawVal {
    type Error = ConversionError;

    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<String, Self::Error> {
        <_ as TryFromVal<E, RawVal>>::try_from_val(env, self)
    }
}

impl<E: Env> IntoVal<E, RawVal> for &str {
    #[inline(always)]
    fn into_val(self, env: &E) -> RawVal {
        env.bytes_new_from_slice(self.as_bytes()).to_raw()
    }
}

impl<E: Env> IntoVal<E, RawVal> for String {
    #[inline(always)]
    fn into_val(self, env: &E) -> RawVal {
        <_ as IntoVal<E, RawVal>>::into_val(&self, env)
    }
}

impl<E: Env> IntoVal<E, RawVal> for &String {
    #[inline(always)]
    fn into_val(self, env: &E) -> RawVal {
        <&str as IntoVal<E, RawVal>>::into_val(&self, env)
    }
}
