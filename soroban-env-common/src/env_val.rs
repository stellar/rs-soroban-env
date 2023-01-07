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

pub trait FromVal<V, E: Env>: Sized {
    fn from_val(env: &E, v: V) -> Self;
}

pub trait TryFromVal<V, E: Env>: Sized {
    type Error;
    fn try_from_val(env: &E, v: V) -> Result<Self, Self::Error>;
}

/// As with the design in the Rust stdlib's Into type, the IntoVal
/// trait is defined as a convenience form with a blanket impl that
/// calls into the corresponding FromVal impl.
pub trait IntoVal<V, E: Env>: Sized {
    fn into_val(self, env: &E) -> V;
}

impl<T, F, E: Env> IntoVal<T, E> for F
where
    T: FromVal<F, E>,
{
    fn into_val(self, env: &E) -> T {
        T::from_val(env, self)
    }
}

/// As with the design in the Rust stdlib's Into type, the TryIntoVal
/// trait is defined as a convenience form with a blanket impl that
/// calls into the corresponding TryFromVal impl.
pub trait TryIntoVal<V, E: Env>: Sized {
    type Error;
    fn try_into_val(self, env: &E) -> Result<V, Self::Error>;
}

impl<T, F, E: Env> TryIntoVal<T, E> for F
where
    T: TryFromVal<F, E>,
{
    type Error = T::Error;

    fn try_into_val(self, env: &E) -> Result<T, Self::Error> {
        T::try_from_val(env, self)
    }
}

// In the Rust stdlib's TryFrom type, an infallible conversion is semantically
// equivalent to a fallible conversion with an uninhabited error type.
//
// Unfortunately since all other impls of TryFromVal are blankets -- unlike most
// concrete TryFrom impls in the Rust stdlib, our impls are generic in E:Env --
// we can't have this blanket impl here for any generic type T, as it will be
// considered as colliding with all other blankets. Blankets are apparently
// considered for collision against one another but not concrete impls. The
// orphan rules are subtle.

/* impl<T, F, E:Env> TryFromVal<F, E> for T
where
    F: IntoVal<T, E>
{
    type Error = Infallible;

    fn try_from_val(env: &E, v: F) -> Result<Self, Self::Error> {
        Ok(F::into_val(v, env))
    }
}
 */

// Similarly, we'd like to have that an environment-free `F: Into<T>` implies
// `T: FromVal<F, E>` that ignores its environment, but this will collide with
// all other generic FromVal impls.
/*
impl<E:Env, T, F> FromVal<F, E> for T
where
    F: Into<T>
{
    fn from_val(_env: &E, v: F) -> Self {
        v.into()
    }
}
*/

// Similarly, we'd like to have that an environment-free `F: TryInto<T>`
// implies `T: TryFromVal<F, E>` that ignores its environment, but this
// will collide with all other generic TryFromVal impls.
/*
impl<E:Env, T, F> TryFromVal<F, E> for T
where
    F: TryInto<T>
{
    type Error = F::Error;

    fn try_from_val(_env: &E, v: F) -> Result<Self, Self::Error> {
        v.try_into()
    }
}
*/

pub(crate) fn log_err_convert<T>(env: &impl Env, val: &impl AsRef<RawVal>) {
    // Logging here is best-effort; ignore failures (they only arise if we're
    // out of gas or something otherwise-unrecoverable).
    let _ = env.log_static_fmt_val_static_str(
        "can't convert {} to {}",
        *val.as_ref(),
        core::any::type_name::<T>(),
    );
}

impl<E: Env> TryFromVal<RawVal, E> for i64 {
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
// This is only required for the UDT code to call, users should
// call FromVal since it's not fallible.
impl<E: Env> TryFromVal<i64, E> for RawVal {
    type Error = ConversionError;
    fn try_from_val(env: &E, v: i64) -> Result<Self, Self::Error> {
        Ok(v.into_val(env))
    }
}
impl<E: Env> FromVal<i64, E> for RawVal {
    fn from_val(env: &E, v: i64) -> Self {
        if v >= 0 {
            unsafe { RawVal::unchecked_from_u63(v) }
        } else {
            env.obj_from_i64(v).to_raw()
        }
    }
}

impl<E: Env> TryFromVal<RawVal, E> for u64 {
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

impl<E: Env> FromVal<u64, E> for RawVal {
    fn from_val(env: &E, v: u64) -> Self {
        env.obj_from_u64(v).to_raw()
    }
}

// Innermost conversions: infallible {ui}128 -> Object and
// fallible Object -> {ui}128
impl<E: Env> FromVal<u128, E> for Object {
    fn from_val(env: &E, v: u128) -> Self {
        env.obj_from_u128_pieces(v as u64, (v >> 64) as u64)
    }
}

impl<E: Env> FromVal<i128, E> for Object {
    fn from_val(env: &E, v: i128) -> Self {
        env.obj_from_i128_pieces(v as u64, (v as u128 >> 64) as u64)
    }
}

impl<E: Env> TryFromVal<Object, E> for u128 {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: Object) -> Result<Self, Self::Error> {
        let lo = env.obj_to_u128_lo64(v);
        let hi = env.obj_to_u128_hi64(v);
        let u: u128 = (lo as u128) | ((hi as u128) << 64);
        Ok(u)
    }
}

impl<E: Env> TryFromVal<Object, E> for i128 {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: Object) -> Result<Self, Self::Error> {
        let lo = env.obj_to_i128_lo64(v);
        let hi = env.obj_to_i128_hi64(v);
        let u: u128 = (lo as u128) | ((hi as u128) << 64);
        Ok(u as i128)
    }
}

macro_rules! decl_rawval_conversions_via_object {
    ($T:ty) => {
        impl<E: Env> TryFromVal<RawVal, E> for $T {
            type Error = ConversionError;

            fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
                let ob: Object = val.try_into()?;
                ob.try_into_val(env)
            }
        }
        // This is only required for the UDT code to call, users should
        // call FromVal since it's not fallible.
        impl<E: Env> TryFromVal<$T, E> for RawVal {
            type Error = ConversionError;
            fn try_from_val(env: &E, v: $T) -> Result<Self, Self::Error> {
                let ob: Object = v.into_val(env);
                Ok(ob.to_raw())
            }
        }
        impl<E: Env> FromVal<$T, E> for RawVal {
            fn from_val(env: &E, v: $T) -> Self {
                let ob: Object = v.into_val(env);
                ob.to_raw()
            }
        }
    };
}

// TODO: we should arrange to convert all object types to and from Object and
// RawVal the same way; u128 and i128 work differently than the others here --
// fallibility differs, the fast path differs, the use of delegation differs --
// which is wrong. Not fatally wrong, but pointless difference.
decl_rawval_conversions_via_object!(u128);
decl_rawval_conversions_via_object!(i128);

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<RawVal, E> for ScVal
where
    ScObject: TryFromVal<Object, E>,
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
impl<E: Env> TryFromVal<&ScVal, E> for RawVal
where
    for<'a> &'a ScObject: TryIntoVal<Object, E>,
{
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &ScVal) -> Result<Self, Self::Error> {
        Ok(match v {
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
impl<E: Env> TryFromVal<ScVal, E> for RawVal
where
    for<'a> &'a ScObject: TryIntoVal<Object, E>,
{
    type Error = ConversionError;

    fn try_from_val(env: &E, v: ScVal) -> Result<Self, Self::Error> {
        (&v).try_into_val(env)
    }
}
