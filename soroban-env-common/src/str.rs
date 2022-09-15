#![cfg(feature = "std")]

use stellar_xdr::ScObjectType;

use crate::{ConversionError, Env, Object, RawVal, RawValConvertible, TryFromVal, TryIntoVal};

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

// impl<E: Env, T, F> TryIntoVal<E, Result<T, F>> for RawVal
// where
//     T: TryFromVal<E, RawVal, Error = ConversionError>,
//     F: TryFromVal<E, RawVal, Error = ConversionError>,
// {
//     type Error = ConversionError;

//     #[inline(always)]
//     fn try_into_val(self, env: &E) -> Result<Result<T, F>, Self::Error> {
//         <_ as TryFromVal<E, RawVal>>::try_from_val(env, self)
//     }
// }

// impl<E: Env, T, F> IntoVal<E, RawVal> for Result<T, F>
// where
//     T: IntoVal<E, RawVal>,
//     F: IntoVal<E, RawVal>,
// {
//     #[inline(always)]
//     fn into_val(self, env: &E) -> RawVal {
//         match self {
//             Ok(t) => (SYMBOL_OK, t).into_val(env),
//             Err(f) => (SYMBOL_ERROR, f).into_val(env),
//         }
//     }
// }

// impl<E: Env, T, F> IntoVal<E, RawVal> for &Result<T, F>
// where
//     for<'a> &'a T: IntoVal<E, RawVal>,
//     for<'a> &'a F: IntoVal<E, RawVal>,
// {
//     #[inline(always)]
//     fn into_val(self, env: &E) -> RawVal {
//         match self {
//             Ok(t) => (SYMBOL_OK, t).into_val(env),
//             Err(f) => (SYMBOL_ERROR, f).into_val(env),
//         }
//     }
// }
