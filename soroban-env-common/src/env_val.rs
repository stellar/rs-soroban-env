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
    // Logging here is best-effort; ignore failures (they only arise if we're
    // out of gas or something otherwise-unrecoverable).
    let _ = env.log_static_fmt_val_static_str(
        "can't convert {} to {}",
        *val.as_ref(),
        core::any::type_name::<T>(),
    );
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for ScVal
where
    ScObject: TryFromVal<E, Object>,
{
    type Error = ConversionError;

    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        match val.get_tag() {
            Tag::U32 => Ok(ScVal::U32(unsafe {
                <u32 as RawValConvertible>::unchecked_from_val(val)
            })),
            Tag::I32 => Ok(ScVal::I32(unsafe {
                <i32 as RawValConvertible>::unchecked_from_val(val)
            })),
            // TODO v160: move u64 and i64 from SCObject to SCVal
            Tag::U64 | Tag::I64 => todo!(),
            Tag::Static => {
                let tag_static = unsafe { <Static as RawValConvertible>::unchecked_from_val(val) };
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
                let sym: Symbol = unsafe { <Symbol as RawValConvertible>::unchecked_from_val(val) };
                let str: String = sym.into_iter().collect();
                Ok(ScVal::Symbol(str.as_bytes().try_into()?))
            }
            Tag::BitSet => {
                // FIXME v160: make ScVal::Bitset able to take 128 bits?
                Ok(ScVal::Bitset(val.payload as u64))
            }
            Tag::Status => {
                let status: Status =
                    unsafe { <Status as RawValConvertible>::unchecked_from_val(val) };
                Ok(status.try_into()?)
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
            ScVal::U63(i) => (*i).into(),
            ScVal::U32(u) => (*u).into(),
            ScVal::I32(i) => (*i).into(),
            ScVal::Static(ScStatic::Void) => RawVal::from_void(),
            ScVal::Static(ScStatic::True) => RawVal::from_bool(true),
            ScVal::Static(ScStatic::False) => RawVal::from_bool(false),
            ScVal::Static(other) => RawVal::from_other_static(*other),
            ScVal::Object(None) => return Err(ConversionError),
            ScVal::Object(Some(ob)) => ob.try_into_val(env).map_err(|_| ConversionError)?.into(),
            ScVal::Symbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(ConversionError),
                };
                Symbol::try_from_str(ss)?.into()
            }
            ScVal::Bitset(i) => {
                let bs: BitSet = (*i as u128).into();
                bs.into()
            }
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

impl<E: Env + Debug, V: Debug> Debug for EnvVal<E, V> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("EnvVal")
            .field("env", &self.env)
            .field("val", &self.val)
            .finish()
    }
}

impl<E: Env> Eq for EnvVal<E, Object> {}
impl<E: Env> PartialEq for EnvVal<E, Object> {
    fn eq(&self, other: &Self) -> bool {
        self.env.check_same_env(&other.env);
        if self.val.get_handle() == other.val.get_handle() {
            true
        } else {
            let v = self.env.obj_cmp(self.val, other.val);
            v == 0
        }
    }
}

impl<E: Env, V: Val + Eq> Eq for EnvVal<E, V> {}
impl<E: Env, V: Val + Eq> PartialEq for EnvVal<E, V> {
    fn eq(&self, other: &Self) -> bool {
        self.env.check_same_env(&other.env);
        self.val.eq(&other.val)
    }
}

impl<E: Env> Eq for EnvVal<E, RawVal> {}
impl<E: Env> PartialEq for EnvVal<E, RawVal> {
    fn eq(&self, other: &Self) -> bool {
        self.env.check_same_env(&other.env);
        if (self.val.control == other.val.control) && (self.val.payload == other.val.payload) {
            // Fast path: bit-identical vals.
            true
        } else if self.val.get_tag() != Tag::Object || other.val.get_tag() != Tag::Object {
            // Other fast path: non-identical non-objects, must be non-equal.
            false
        } else {
            // Slow path: deep object comparison via the environment.
            let obj1: Object = unsafe { Object::unchecked_from_val(self.val) };
            let obj2: Object = unsafe { Object::unchecked_from_val(other.val) };
            let v = self.env.obj_cmp(obj1, obj2);
            v == 0
        }
    }
}

impl<E: Env, V: Val> PartialOrd for EnvVal<E, V>
where
    EnvVal<E, V>: Eq,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<E: Env, V: Val> Ord for EnvVal<E, V>
where
    EnvVal<E, V>: Eq,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.env.check_same_env(&other.env);
        let rv1: RawVal = self.val.clone().into();
        let rv2: RawVal = other.val.clone().into();
        let self_tag = rv1.get_tag();
        let other_tag = rv2.get_tag();
        if self_tag < other_tag {
            Ordering::Less
        } else if self_tag > other_tag {
            Ordering::Greater
        } else {
            // Tags are equal so we only have to switch on one.
            match self_tag {
                Tag::U32 => {
                    let a = unsafe { <u32 as RawValConvertible>::unchecked_from_val(rv1) };
                    let b = unsafe { <u32 as RawValConvertible>::unchecked_from_val(rv2) };
                    a.cmp(&b)
                }
                Tag::I32 => {
                    let a = unsafe { <i32 as RawValConvertible>::unchecked_from_val(rv1) };
                    let b = unsafe { <i32 as RawValConvertible>::unchecked_from_val(rv2) };
                    a.cmp(&b)
                }
                Tag::U64 => {
                    let a = unsafe { <u64 as RawValConvertible>::unchecked_from_val(rv1) };
                    let b = unsafe { <u64 as RawValConvertible>::unchecked_from_val(rv2) };
                    a.cmp(&b)
                }
                Tag::I64 => {
                    let a = unsafe { <i64 as RawValConvertible>::unchecked_from_val(rv1) };
                    let b = unsafe { <i64 as RawValConvertible>::unchecked_from_val(rv2) };
                    a.cmp(&b)
                }
                Tag::Static => rv1.payload.cmp(&rv2.payload),
                Tag::Object => {
                    let obj1: Object = unsafe { Object::unchecked_from_val(rv1) };
                    let obj2: Object = unsafe { Object::unchecked_from_val(rv2) };
                    let v = self.env.obj_cmp(obj1, obj2);
                    if v == 0 {
                        Ordering::Equal
                    } else if v < 0 {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                }
                Tag::Symbol => {
                    let a = unsafe { <Symbol as RawValConvertible>::unchecked_from_val(rv1) };
                    let b = unsafe { <Symbol as RawValConvertible>::unchecked_from_val(rv2) };
                    a.cmp(&b)
                }
                Tag::BitSet => {
                    let a = unsafe { <BitSet as RawValConvertible>::unchecked_from_val(rv1) };
                    let b = unsafe { <BitSet as RawValConvertible>::unchecked_from_val(rv2) };
                    a.cmp(&b)
                }
                Tag::Status => {
                    let a = unsafe { <Status as RawValConvertible>::unchecked_from_val(rv1) };
                    let b = unsafe { <Status as RawValConvertible>::unchecked_from_val(rv2) };
                    a.cmp(&b)
                }
            }
        }
    }
}
