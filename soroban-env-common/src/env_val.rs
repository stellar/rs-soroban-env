use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use stellar_xdr::{ScObject, ScStatic, ScVal};

use crate::{raw_val::ConversionError, Object};
#[cfg(feature = "std")]
use crate::{BitSet, Static, Status, Symbol, Tag};

use super::{
    raw_val::{RawVal, RawValConvertible},
    Env,
};

pub trait IntoVal<E: Env, V>: Sized {
    fn into_val(self, env: &E) -> V;
}

pub trait TryIntoVal<E: Env, V>: Sized {
    type Error;
    fn try_into_val(self, env: &E) -> Result<V, Self::Error>;
}

pub trait FromVal<E: Env, V>: Sized {
    fn from_val(env: &E, v: V) -> Self;
}

pub trait TryFromVal<E: Env, V>: Sized {
    type Error;
    fn try_from_val(env: &E, v: V) -> Result<Self, Self::Error>;
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

impl<E: Env> TryFromVal<E, RawVal> for i64 {
    type Error = ConversionError;

    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        if val.is_u63() {
            Ok(unsafe { val.unchecked_as_u63() })
        } else if Object::val_is_obj_type(val, ScObjectType::I64) {
            let obj = unsafe { Object::unchecked_from_val(val) };
            Ok(env.obj_to_i64(obj))
        } else {
            log_err_convert::<i64>(env, &val);
            Err(ConversionError)
        }
    }
}

impl<E: Env> IntoVal<E, RawVal> for i64 {
    fn into_val(self, env: &E) -> RawVal {
        if self >= 0 {
            unsafe { RawVal::unchecked_from_u63(self) }
        } else {
            env.obj_from_i64(self).to_raw()
        }
    }
}

impl<E: Env> IntoVal<E, RawVal> for &i64 {
    fn into_val(self, env: &E) -> RawVal {
        (*self).into_val(env)
    }
}

impl<E: Env> TryIntoVal<E, RawVal> for i64 {
    type Error = ConversionError;
    fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
        Ok(IntoVal::into_val(self, env))
    }
}

impl<E: Env> TryIntoVal<E, i64> for RawVal {
    type Error = <i64 as TryFromVal<E, RawVal>>::Error;
    fn try_into_val(self, env: &E) -> Result<i64, Self::Error> {
        <_ as TryFromVal<_, _>>::try_from_val(env, self)
    }
}

impl<E: Env> TryFromVal<E, RawVal> for u64 {
    type Error = ConversionError;

    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        if Object::val_is_obj_type(val, ScObjectType::U64) {
            let obj = unsafe { Object::unchecked_from_val(val) };
            Ok(env.obj_to_u64(obj))
        } else {
            log_err_convert::<u64>(env, &val);
            Err(ConversionError)
        }
    }
}

impl<E: Env> IntoVal<E, RawVal> for u64 {
    fn into_val(self, env: &E) -> RawVal {
        env.obj_from_u64(self).to_raw()
    }
}

impl<E: Env> IntoVal<E, RawVal> for &u64 {
    fn into_val(self, env: &E) -> RawVal {
        (*self).into_val(env)
    }
}

impl<E: Env> TryIntoVal<E, RawVal> for u64 {
    type Error = ConversionError;
    fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
        Ok(IntoVal::into_val(self, env))
    }
}

impl<E: Env> TryIntoVal<E, u64> for RawVal {
    type Error = <u64 as TryFromVal<E, RawVal>>::Error;
    fn try_into_val(self, env: &E) -> Result<u64, Self::Error> {
        <_ as TryFromVal<_, _>>::try_from_val(env, self)
    }
}

// Innermost conversions: infallible {ui}128 -> Object and
// fallible Object -> {ui}128
impl<E: Env> IntoVal<E, Object> for u128 {
    fn into_val(self, env: &E) -> Object {
        env.obj_from_u128_pieces(self as u64, (self >> 64) as u64)
    }
}

impl<E: Env> IntoVal<E, Object> for i128 {
    fn into_val(self, env: &E) -> Object {
        env.obj_from_i128_pieces(self as u64, (self as u128 >> 64) as u64)
    }
}

impl<E: Env> TryIntoVal<E, u128> for Object {
    type Error = ConversionError;

    fn try_into_val(self, env: &E) -> Result<u128, Self::Error> {
        let lo = env.obj_to_u128_lo64(self);
        let hi = env.obj_to_u128_hi64(self);
        let u: u128 = (lo as u128) | ((hi as u128) << 64);
        Ok(u)
    }
}

impl<E: Env> TryIntoVal<E, i128> for Object {
    type Error = ConversionError;

    fn try_into_val(self, env: &E) -> Result<i128, Self::Error> {
        let lo = env.obj_to_i128_lo64(self);
        let hi = env.obj_to_i128_hi64(self);
        let u: u128 = (lo as u128) | ((hi as u128) << 64);
        Ok(u as i128)
    }
}

macro_rules! decl_int128_conversions {
    ($T:ty) => {
        // RawVal-typed versions delegate to Object-typed
        impl<E: Env> IntoVal<E, RawVal> for $T {
            fn into_val(self, env: &E) -> RawVal {
                <Self as IntoVal<E, Object>>::into_val(self, env).to_raw()
            }
        }
        impl<E: Env> TryIntoVal<E, $T> for RawVal {
            type Error = ConversionError;
            fn try_into_val(self, env: &E) -> Result<$T, Self::Error> {
                let ob: Object = self.try_into_val(env)?;
                <Object as TryIntoVal<E, $T>>::try_into_val(ob, env)
            }
        }
        // Infallible reference-types delegate to deref
        impl<E: Env> IntoVal<E, Object> for &$T {
            fn into_val(self, env: &E) -> Object {
                (*self).into_val(env)
            }
        }
        impl<E: Env> IntoVal<E, RawVal> for &$T {
            fn into_val(self, env: &E) -> RawVal {
                (*self).into_val(env)
            }
        }
        // Fallibe versions of infallibe injections just delegate to them.
        impl<E: Env> TryIntoVal<E, RawVal> for $T {
            type Error = ConversionError;

            fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
                Ok(<Self as IntoVal<E, RawVal>>::into_val(self, env).into())
            }
        }

        impl<E: Env> TryIntoVal<E, RawVal> for &$T {
            type Error = ConversionError;

            fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
                Ok(<Self as IntoVal<E, RawVal>>::into_val(self, env).into())
            }
        }

        impl<E: Env> TryIntoVal<E, Object> for $T {
            type Error = ConversionError;

            fn try_into_val(self, env: &E) -> Result<Object, Self::Error> {
                Ok(<Self as IntoVal<E, Object>>::into_val(self, env).into())
            }
        }

        impl<E: Env> TryIntoVal<E, Object> for &$T {
            type Error = ConversionError;

            fn try_into_val(self, env: &E) -> Result<Object, Self::Error> {
                Ok(<Self as IntoVal<E, Object>>::into_val(self, env).into())
            }
        }
        // TryFrom impls delegate to TryInto impls the other direction
        impl<E: Env> TryFromVal<E, RawVal> for $T {
            type Error = ConversionError;

            fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
                <RawVal as TryIntoVal<E, $T>>::try_into_val(val, env)
            }
        }
        impl<E: Env> TryFromVal<E, Object> for $T {
            type Error = ConversionError;

            fn try_from_val(env: &E, val: Object) -> Result<Self, Self::Error> {
                <Object as TryIntoVal<E, $T>>::try_into_val(val, env)
            }
        }
    };
}

decl_int128_conversions!(u128);
decl_int128_conversions!(i128);

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for ScVal
where
    ScObject: TryFromVal<E, Object>,
{
    type Error = ConversionError;

    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        if val.is_u63() {
            Ok(ScVal::U63(unsafe { val.unchecked_as_u63() }))
        } else {
            match val.get_tag() {
                Tag::U32 => Ok(ScVal::U32(unsafe {
                    <u32 as RawValConvertible>::unchecked_from_val(val)
                })),
                Tag::I32 => Ok(ScVal::I32(unsafe {
                    <i32 as RawValConvertible>::unchecked_from_val(val)
                })),
                Tag::Static => {
                    let tag_static =
                        unsafe { <Static as RawValConvertible>::unchecked_from_val(val) };
                    if tag_static.is_type(ScStatic::True) {
                        Ok(ScVal::Static(ScStatic::True))
                    } else if tag_static.is_type(ScStatic::False) {
                        Ok(ScVal::Static(ScStatic::False))
                    } else if tag_static.is_type(ScStatic::Void) {
                        Ok(ScVal::Static(ScStatic::Void))
                    } else if tag_static.is_type(ScStatic::LedgerKeyContractCode) {
                        Ok(ScVal::Static(ScStatic::LedgerKeyContractCode))
                    } else {
                        log_err_convert::<Self>(env, &val);
                        Err(ConversionError)
                    }
                }
                Tag::Object => unsafe {
                    let ob = <Object as RawValConvertible>::unchecked_from_val(val);
                    let scob = ScObject::try_from_val(&env, ob).map_err(|_| ConversionError)?;
                    Ok(ScVal::Object(Some(scob)))
                },
                Tag::Symbol => {
                    let sym: Symbol =
                        unsafe { <Symbol as RawValConvertible>::unchecked_from_val(val) };
                    let str: String = sym.into_iter().collect();
                    Ok(ScVal::Symbol(str.as_bytes().try_into()?))
                }
                Tag::BitSet => Ok(ScVal::Bitset(val.get_payload())),
                Tag::Status => {
                    let status: Status =
                        unsafe { <Status as RawValConvertible>::unchecked_from_val(val) };
                    Ok(status.try_into()?)
                }
                Tag::Reserved => Err(ConversionError),
            }
        }
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryIntoVal<E, ScVal> for RawVal
where
    ScObject: TryFromVal<E, Object>,
{
    type Error = <ScVal as TryFromVal<E, RawVal>>::Error;

    fn try_into_val(self, env: &E) -> Result<ScVal, Self::Error> {
        <_ as TryFromVal<E, RawVal>>::try_from_val(env, self)
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryIntoVal<E, RawVal> for &ScVal
where
    for<'a> &'a ScObject: TryIntoVal<E, Object>,
{
    type Error = ConversionError;
    fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
        Ok(match self {
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
            ScVal::Object(Some(ob)) => ob.try_into_val(env).map_err(|_| ConversionError)?.to_raw(),
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

#[cfg(feature = "std")]
impl<E: Env> TryIntoVal<E, RawVal> for ScVal
where
    for<'a> &'a ScObject: TryIntoVal<E, Object>,
{
    type Error = ConversionError;
    fn try_into_val(self, env: &E) -> Result<RawVal, Self::Error> {
        (&self).try_into_val(env)
    }
}
