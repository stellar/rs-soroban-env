use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use stellar_xdr::{ScObject, ScStatic, ScVal};

use crate::{raw_val::ConversionError, MapErrToEnv, Object};
#[cfg(feature = "std")]
use crate::{BitSet, Static, Status, Symbol, Tag};

use super::{
    raw_val::{RawVal, RawValConvertible},
    Env,
};

/// General trait representing a the ability of some object to perform a
/// (possibly unsuccessful) conversion between two other types.
pub trait Convert<F, T>: Env {
    fn convert(&self, f: F) -> Result<T, Self::Error>;
}

pub trait TryIntoVal<E: Env, V> {
    fn try_into_val(&self, env: &E) -> Result<V, E::Error>;
}

pub trait TryFromVal<E: Env, V: ?Sized>: Sized {
    fn try_from_val(env: &E, v: &V) -> Result<Self, E::Error>;
}

// Blanket-impl uses of TryIntoVal to TryFromVal, so that we
// only ever have to impl TryFromVal.
impl<E: Env, T, U> TryIntoVal<E, T> for U
where
    T: TryFromVal<E, U>,
{
    fn try_into_val(&self, env: &E) -> Result<T, E::Error> {
        T::try_from_val(env, self)
    }
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

// i64 conversions

impl<E: Env> TryFromVal<E, RawVal> for i64 {
    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, E::Error> {
        let val = *val;
        if val.is_u63() {
            Ok(unsafe { val.unchecked_as_u63() })
        } else if Object::val_is_obj_type(val, ScObjectType::I64) {
            let obj = unsafe { Object::unchecked_from_val(val) };
            env.obj_to_i64(obj)
        } else {
            log_err_convert::<i64>(env, &val);
            Err(ConversionError).map_err_to_env(env)
        }
    }
}

impl<E: Env> TryFromVal<E, i64> for RawVal {
    fn try_from_val(env: &E, v: &i64) -> Result<Self, E::Error> {
        let v = *v;
        if v >= 0 {
            Ok(unsafe { RawVal::unchecked_from_u63(v) })
        } else {
            Ok(env.obj_from_i64(v)?.to_raw())
        }
    }
}

// u64 conversions

impl<E: Env> TryFromVal<E, RawVal> for u64 {
    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, E::Error> {
        let val = *val;
        if Object::val_is_obj_type(val, ScObjectType::U64) {
            let obj = unsafe { Object::unchecked_from_val(val) };
            env.obj_to_u64(obj)
        } else {
            log_err_convert::<u64>(env, &val);
            Err(ConversionError).map_err_to_env(env)
        }
    }
}

impl<E: Env> TryFromVal<E, u64> for RawVal {
    fn try_from_val(env: &E, v: &u64) -> Result<Self, E::Error> {
        Ok(env.obj_from_u64(*v)?.to_raw())
    }
}

// i128 conversions

impl<E: Env> TryFromVal<E, RawVal> for i128 {
    fn try_from_val(env: &E, v: &RawVal) -> Result<Self, E::Error> {
        let v = *v;
        let obj = v.try_into().map_err_to_env(env)?;
        let lo = env.obj_to_i128_lo64(obj)?;
        let hi = env.obj_to_i128_hi64(obj)?;
        let u: u128 = (lo as u128) | ((hi as u128) << 64);
        Ok(u as i128)
    }
}
impl<E: Env> TryFromVal<E, i128> for RawVal {
    fn try_from_val(env: &E, v: &i128) -> Result<Self, E::Error> {
        let v = *v;
        Ok(env
            .obj_from_i128_pieces(v as u64, (v as u128 >> 64) as u64)?
            .into())
    }
}

// u128 conversions

impl<E: Env> TryFromVal<E, RawVal> for u128 {
    fn try_from_val(env: &E, v: &RawVal) -> Result<Self, E::Error> {
        let v = *v;
        let obj = v.try_into().map_err_to_env(env)?;
        let lo = env.obj_to_u128_lo64(obj)?;
        let hi = env.obj_to_u128_hi64(obj)?;
        let u: u128 = (lo as u128) | ((hi as u128) << 64);
        Ok(u)
    }
}
impl<E: Env> TryFromVal<E, u128> for RawVal {
    fn try_from_val(env: &E, v: &u128) -> Result<Self, E::Error> {
        let v = *v;
        Ok(env.obj_from_u128_pieces(v as u64, (v >> 64) as u64)?.into())
    }
}

// ScVal conversions (that require ScObject conversions)

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for ScVal
where
    ScObject: TryFromVal<E, Object>,
{
    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, E::Error> {
        let val = *val;
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
                        Err(ConversionError).map_err_to_env(env)
                    }
                }
                Tag::Object => unsafe {
                    let ob = <Object as RawValConvertible>::unchecked_from_val(val);
                    let scob = ScObject::try_from_val(&env, &ob)?;
                    Ok(ScVal::Object(Some(scob)))
                },
                Tag::Symbol => {
                    let sym: Symbol =
                        unsafe { <Symbol as RawValConvertible>::unchecked_from_val(val) };
                    let str: String = sym.into_iter().collect();
                    Ok(ScVal::Symbol(
                        str.as_bytes().try_into().map_err_to_env(env)?,
                    ))
                }
                Tag::BitSet => Ok(ScVal::Bitset(val.get_payload())),
                Tag::Status => {
                    let status: Status =
                        unsafe { <Status as RawValConvertible>::unchecked_from_val(val) };
                    Ok(status.try_into().map_err_to_env(env)?)
                }
                Tag::Reserved => Err(ConversionError).map_err_to_env(env),
            }
        }
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, ScVal> for RawVal
where
    Object: TryFromVal<E, ScObject>,
{
    fn try_from_val(env: &E, val: &ScVal) -> Result<Self, E::Error> {
        Ok(match val {
            ScVal::U63(i) => {
                if *i >= 0 {
                    unsafe { RawVal::unchecked_from_u63(*i) }
                } else {
                    return Err(ConversionError).map_err_to_env(env);
                }
            }
            ScVal::U32(u) => (*u).into(),
            ScVal::I32(i) => (*i).into(),
            ScVal::Static(ScStatic::Void) => RawVal::from_void(),
            ScVal::Static(ScStatic::True) => RawVal::from_bool(true),
            ScVal::Static(ScStatic::False) => RawVal::from_bool(false),
            ScVal::Static(other) => RawVal::from_other_static(*other),
            ScVal::Object(None) => return Err(ConversionError).map_err_to_env(env),
            ScVal::Object(Some(ob)) => Object::try_from_val(env, ob)?.to_raw(),
            ScVal::Symbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(ConversionError).map_err_to_env(env),
                };
                Symbol::try_from_str(ss).map_err_to_env(env)?.into()
            }
            ScVal::Bitset(i) => BitSet::try_from_u64(*i).map_err_to_env(env)?.into(),
            ScVal::Status(st) => st.into(),
        })
    }
}
