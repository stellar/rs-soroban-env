use core::convert::Infallible;

use stellar_xdr::{ScObject, ScStatic, ScVal, AccountId, ScContractCode};

use crate::{
    BitSet, CheckedEnv, ConversionError, Env, Object, RawVal, RawValConvertible, Static, Status,
    Symbol, Tag,
};

/// General trait representing a the ability of some object to perform a
/// (possibly unsuccessful) conversion between two other types. Provides
/// both by-ref and by-owning conversions, with the default by-owning
/// implementation calling the by-ref, then dropping the owned input.
pub trait Convert<F, T> {
    type Error;
    fn convert(&self, f: F) -> Result<T, Self::Error> {
        self.convert_ref(&f)
    }
    fn convert_ref(&self, f: &F) -> Result<T, Self::Error>;
}

pub(crate) fn log_err_convert<T>(env: &impl Env, val: &impl AsRef<RawVal>) {
    // Logging here is best-effort; ignore failures (they only arise if we're
    // out of gas or something otherwise-unrecoverable).
    let _ = env.log_static_fmt_val_static_str(
        "can't convert {} to {}",
        *val.as_ref(),
        core::any::type_name::<T>(),
    );
}

#[cfg(feature = "std")]
impl<E> Convert<RawVal, ScVal> for E
where
    E: Env + Convert<Object, ScObject>,
{
    type Error = ConversionError;

    fn convert_ref(&self, val: &RawVal) -> Result<ScVal, Self::Error> {
        if val.is_u63() {
            Ok(ScVal::U63(unsafe { val.unchecked_as_u63() }))
        } else {
            match val.get_tag() {
                Tag::U32 => Ok(ScVal::U32(unsafe { u32::unchecked_from_val(*val) })),
                Tag::I32 => Ok(ScVal::I32(unsafe { i32::unchecked_from_val(*val) })),
                Tag::Static => {
                    let tag_static = unsafe { Static::unchecked_from_val(*val) };
                    if tag_static.is_type(ScStatic::True) {
                        Ok(ScVal::Static(ScStatic::True))
                    } else if tag_static.is_type(ScStatic::False) {
                        Ok(ScVal::Static(ScStatic::False))
                    } else if tag_static.is_type(ScStatic::Void) {
                        Ok(ScVal::Static(ScStatic::Void))
                    } else if tag_static.is_type(ScStatic::LedgerKeyContractCode) {
                        Ok(ScVal::Static(ScStatic::LedgerKeyContractCode))
                    } else {
                        log_err_convert::<Self>(self, val);
                        Err(ConversionError)
                    }
                }
                Tag::Object => unsafe {
                    let ob = Object::unchecked_from_val(*val);
                    let scob = self.convert(ob).map_err(|_| ConversionError)?;
                    Ok(ScVal::Object(Some(scob)))
                },
                Tag::Symbol => {
                    let sym: Symbol = unsafe { Symbol::unchecked_from_val(*val) };
                    let str: String = sym.into_iter().collect();
                    Ok(ScVal::Symbol(str.as_bytes().try_into()?))
                }
                Tag::BitSet => Ok(ScVal::Bitset(val.get_payload())),
                Tag::Status => {
                    let status: Status = unsafe { Status::unchecked_from_val(*val) };
                    Ok(status.try_into()?)
                }
                Tag::Reserved => Err(ConversionError),
            }
        }
    }
}

#[cfg(feature = "std")]
impl<E> Convert<ScVal, RawVal> for E
where
    E: Env + Convert<ScObject, Object>,
{
    type Error = ConversionError;

    fn convert_ref(&self, f: &ScVal) -> Result<RawVal, Self::Error> {
        Ok(match f {
            ScVal::U63(i) => {
                if *i >= 0 {
                    unsafe { RawVal::unchecked_from_u63(*i) }
                } else {
                    return Err(ConversionError);
                }
            }
            ScVal::U32(u) => (*u).into(),
            ScVal::I32(i) => (*i).into(),
            ScVal::Static(ScStatic::Void) => RawVal::from_void(),
            ScVal::Static(ScStatic::True) => RawVal::from_bool(true),
            ScVal::Static(ScStatic::False) => RawVal::from_bool(false),
            ScVal::Static(other) => RawVal::from_other_static(*other),
            ScVal::Object(None) => return Err(ConversionError),
            ScVal::Object(Some(ob)) => self.convert_ref(ob).map_err(|_| ConversionError)?.to_raw(),
            ScVal::Symbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(ConversionError),
                };
                Symbol::try_from_str(ss)?.into()
            }
            ScVal::Bitset(i) => BitSet::try_from_u64(*i)?.into(),
            ScVal::Status(st) => st.into(),
        })
    }
}

// New CheckedEnv-specific conversions should all look like this (after
// infallible conversions on Env are moved to SDK)
impl<E: CheckedEnv> Convert<u64, Object> for E {
    type Error = E::Error;
    fn convert_ref(&self, f: &u64) -> Result<Object, Self::Error> {
        self.obj_from_u64(*f)
    }
}

impl<E: CheckedEnv> Convert<Object, u64> for E {
    type Error = E::Error;
    fn convert_ref(&self, f: &Object) -> Result<u64, Self::Error> {
        self.obj_to_u64(*f)
    }
}

impl<E: CheckedEnv> Convert<i64, Object> for E {
    type Error = E::Error;
    fn convert_ref(&self, f: &i64) -> Result<Object, Self::Error> {
        self.obj_from_i64(*f)
    }
}

impl<E: CheckedEnv> Convert<Object, i64> for E {
    type Error = E::Error;
    fn convert_ref(&self, f: &Object) -> Result<i64, Self::Error> {
        self.obj_to_i64(*f)
    }
}

// RawVal <-> i64 has the special-case handling for u63
impl<E: CheckedEnv> Convert<i64, RawVal> for E {
    type Error = E::Error;
    fn convert_ref(&self, f: &i64) -> Result<RawVal, Self::Error> {
        if *f >= 0 {
            Ok(unsafe { RawVal::unchecked_from_u63(*f) })
        } else {
            Ok(self.obj_from_i64(*f)?.into())
        }
    }
}

impl<E> Convert<RawVal, i64> for E
where
    E: Convert<Object, i64>,
    E: CheckedEnv<Error = <E as Convert<Object, i64>>::Error>,
    <E as CheckedEnv>::Error: From<ConversionError>,
{
    type Error = <E as Convert<Object, i64>>::Error;
    fn convert_ref(&self, f: &RawVal) -> Result<i64, Self::Error> {
        if f.is_u63() {
            Ok(unsafe { f.unchecked_as_u63() })
        } else {
            let obj: Object = f.try_into()?;
            self.obj_to_i64(obj)
        }
    }
}

impl<E:CheckedEnv> Convert<u128,Object> for E {
    type Error = E::Error;

    fn convert_ref(&self, f: &u128) -> Result<Object, Self::Error> {
        self.obj_from_u128_pieces(*f as u64, (*f >> 64) as u64)
    }
}

impl<E:CheckedEnv> Convert<Object,u128> for E {
    type Error = E::Error;

    fn convert_ref(&self, obj: &Object) -> Result<u128, Self::Error> {
        let lo: u64 = self.obj_to_u128_lo64(*obj)?;
        let hi: u64 = self.obj_to_u128_hi64(*obj)?;
        let u: u128 = (lo as u128) | ((hi as u128) << 64);
        Ok(u)
    }
}

impl<E:CheckedEnv> Convert<i128,Object> for E {
    type Error = E::Error;

    fn convert_ref(&self, f: &i128) -> Result<Object, Self::Error> {
        self.obj_from_i128_pieces(*f as u64, (*f >> 64) as u64)
    }
}

impl<E:CheckedEnv> Convert<Object,i128> for E {
    type Error = E::Error;

    fn convert_ref(&self, obj: &Object) -> Result<i128, Self::Error> {
        let lo: u64 = self.obj_to_i128_lo64(*obj)?;
        let hi: u64 = self.obj_to_i128_hi64(*obj)?;
        let u: u128 = (lo as u128) | ((hi as u128) << 64);
        Ok(u as i128)
    }
}


// Might be able to do this with a blanket on T but so far blankets seem
// to always generate new unexpected conflicts and corner cases.
#[doc(hidden)]
#[macro_export]
macro_rules! decl_convert_object_forwarding_to_rawval {
    ($t:ty) => {
        impl<E> Convert<$t, RawVal> for E
        where
            E: Convert<$t, Object>,
            E: CheckedEnv<Error = <E as Convert<$t, Object>>::Error>,
        {
            type Error = <E as CheckedEnv>::Error;
            fn convert_ref(&self, f: &$t) -> Result<RawVal, Self::Error> {
                Ok(<E as Convert<$t, Object>>::convert_ref(self, f)?.into())
            }
        }
        impl<E> Convert<RawVal, $t> for E
        where
            E: Convert<Object, $t>,
            E: CheckedEnv<Error = <E as Convert<Object, $t>>::Error>,
            <E as CheckedEnv>::Error: From<ConversionError>,
        {
            type Error = <E as CheckedEnv>::Error;
            fn convert_ref(&self, f: &RawVal) -> Result<$t, Self::Error> {
                let obj: Object = f.try_into()?;
                <E as Convert<Object, $t>>::convert_ref(self, &obj)
            }
        }
    };
}

decl_convert_object_forwarding_to_rawval!(u64);
decl_convert_object_forwarding_to_rawval!(u128);
decl_convert_object_forwarding_to_rawval!(i128);
decl_convert_object_forwarding_to_rawval!(AccountId);
decl_convert_object_forwarding_to_rawval!(ScContractCode);
