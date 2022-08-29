use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use stellar_xdr::{ScObject, ScStatic, ScVal};

#[cfg(feature = "std")]
use crate::Static;
use crate::{raw_val::ConversionError, BitSet, Object, Status, Symbol, Tag, Val};

use super::{
    raw_val::{RawVal, RawValConvertible},
    Env,
};
use core::{cmp::Ordering, convert::Infallible, fmt::Debug};

/// Some type of value coupled to a specific instance of [Env], which some of
/// the value's methods may call into to support some conversion and comparison
/// functions.
///
/// The value and `Env` instance used in an `EnvVal` will vary by context:
///
///   - In contract code compiled for WASM, the `Env` is a zero-sized `Guest`
///     struct
///   - In contract code compiled natively for local testing, the `Env` is a
///     reference-counted `Host`
///   - Inside the `Host`, the `Env` is either a `Host` or a weak reference to a
///     `Host`
///
/// The value will typically either be [RawVal] or one of its tag-specific
/// wrapper types.
#[derive(Clone)]
pub struct EnvVal<E: Env, V> {
    /// The environment to call into for comparison and conversion assistance.
    pub env: E,
    /// The value that will call into the environment as needed.
    pub val: V,
}

impl<E: Env, V: Val> EnvVal<E, V> {
    pub fn env(&self) -> &E {
        &self.env
    }
}

impl<E: Env> EnvVal<E, RawVal> {
    pub fn as_raw(&self) -> &RawVal {
        self.val.as_ref()
    }
    pub fn to_raw(&self) -> RawVal {
        self.val
    }
}

impl<E: Env> EnvVal<E, Object> {
    pub fn as_object(&self) -> &Object {
        &self.val
    }
    pub fn to_object(&self) -> Object {
        self.val
    }
}

impl<E: Env, V: Val> AsRef<RawVal> for EnvVal<E, V> {
    fn as_ref(&self) -> &RawVal {
        self.val.as_ref()
    }
}

impl<E: Env, V: Val> AsMut<RawVal> for EnvVal<E, V> {
    fn as_mut(&mut self) -> &mut RawVal {
        self.val.as_mut()
    }
}

pub trait IntoVal<E: Env, V>: Sized {
    fn into_val(self, env: &E) -> V;

    fn into_env_val(self, env: &E) -> EnvVal<E, V> {
        EnvVal {
            env: env.clone(),
            val: self.into_val(env),
        }
    }
}

pub trait TryIntoVal<E: Env, V>: Sized {
    type Error;
    fn try_into_val(self, env: &E) -> Result<V, Self::Error>;

    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, V>, Self::Error> {
        Ok(EnvVal {
            env: env.clone(),
            val: self.try_into_val(env)?,
        })
    }
}

pub trait FromVal<E: Env, V>: Sized {
    fn from_val(env: &E, v: V) -> Self;
}

pub trait TryFromVal<E: Env, V>: Sized {
    type Error;
    fn try_from_val(env: &E, v: V) -> Result<Self, Self::Error>;
}

impl<E: Env> From<EnvVal<E, RawVal>> for RawVal {
    fn from(ev: EnvVal<E, RawVal>) -> Self {
        ev.val
    }
}

impl<E: Env, V> IntoVal<E, V> for EnvVal<E, V> {
    fn into_val(self, _env: &E) -> V {
        self.val
    }
}

impl<E: Env, V> TryFromVal<E, V> for EnvVal<E, V> {
    type Error = Infallible;
    fn try_from_val(env: &E, val: V) -> Result<Self, Self::Error> {
        Ok(EnvVal {
            env: env.clone(),
            val,
        })
    }
}

pub(crate) fn log_err_convert<T>(env: &impl Env, val: &impl AsRef<RawVal>) {
    env.log_static_fmt_val_static_str(
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

impl<E: Env + Debug, V> Debug for EnvVal<E, V> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("EnvVal")
            .field("env", &self.env)
            .field("val", &self)
            .finish()
    }
}

impl<E: Env, V: Val> Eq for EnvVal<E, V> {}

impl<E: Env, V: Val> PartialEq for EnvVal<E, V> {
    fn eq(&self, other: &Self) -> bool {
        self.env.check_same_env(&other.env);
        if self.as_ref().get_payload() == other.as_ref().get_payload() {
            // Fast path: bit-identical vals.
            true
        } else if self.as_ref().get_tag() != Tag::Object || other.as_ref().get_tag() != Tag::Object
        {
            // Other fast path: non-identical non-objects, must be non-equal.
            false
        } else {
            // Slow path: deep object comparison via the environment.
            let v = self.env.obj_cmp(*self.as_ref(), *other.as_ref());
            v == 0
        }
    }
}

impl<E: Env, V: Val> PartialOrd for EnvVal<E, V> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<E: Env, V: Val> Ord for EnvVal<E, V> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.env.check_same_env(&other.env);
        let self_tag = self.as_ref().get_tag();
        let other_tag = other.as_ref().get_tag();
        if self_tag < other_tag {
            Ordering::Less
        } else if self_tag > other_tag {
            Ordering::Greater
        } else {
            // Tags are equal so we only have to switch on one.
            match self_tag {
                Tag::U32 => {
                    let a = unsafe {
                        <u32 as RawValConvertible>::unchecked_from_val(*self.val.as_ref())
                    };
                    let b = unsafe {
                        <u32 as RawValConvertible>::unchecked_from_val(*other.val.as_ref())
                    };
                    a.cmp(&b)
                }
                Tag::I32 => {
                    let a = unsafe {
                        <i32 as RawValConvertible>::unchecked_from_val(*self.val.as_ref())
                    };
                    let b = unsafe {
                        <i32 as RawValConvertible>::unchecked_from_val(*other.val.as_ref())
                    };
                    a.cmp(&b)
                }
                Tag::Static => self
                    .val
                    .as_ref()
                    .get_body()
                    .cmp(&other.val.as_ref().get_body()),
                Tag::Object => {
                    let v = self.env.obj_cmp(*self.val.as_ref(), *other.val.as_ref());
                    if v == 0 {
                        Ordering::Equal
                    } else if v < 0 {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                }
                Tag::Symbol => {
                    let a = unsafe {
                        <Symbol as RawValConvertible>::unchecked_from_val(*self.val.as_ref())
                    };
                    let b = unsafe {
                        <Symbol as RawValConvertible>::unchecked_from_val(*other.val.as_ref())
                    };
                    a.cmp(&b)
                }
                Tag::BitSet => {
                    let a = unsafe {
                        <BitSet as RawValConvertible>::unchecked_from_val(*self.val.as_ref())
                    };
                    let b = unsafe {
                        <BitSet as RawValConvertible>::unchecked_from_val(*other.val.as_ref())
                    };
                    a.cmp(&b)
                }
                Tag::Status => {
                    let a = unsafe {
                        <Status as RawValConvertible>::unchecked_from_val(*self.val.as_ref())
                    };
                    let b = unsafe {
                        <Status as RawValConvertible>::unchecked_from_val(*other.val.as_ref())
                    };
                    a.cmp(&b)
                }
                Tag::Reserved => self
                    .val
                    .as_ref()
                    .get_payload()
                    .cmp(&other.val.as_ref().get_payload()),
            }
        }
    }
}
