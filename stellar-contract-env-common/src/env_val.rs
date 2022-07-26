use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use stellar_xdr::{ScObject, ScStatic, ScVal};

#[cfg(feature = "std")]
use crate::Static;
use crate::{
    raw_val::ConversionError, BitSet, Object, Status, Symbol, Tag, TagType, TaggedVal, Val,
};

use super::{
    raw_val::{RawVal, RawValConvertible},
    Env,
};
use core::{cmp::Ordering, fmt::Debug};

// EnvVal is a value coupled to a specific instance of Env. In the guest we will
// use this with a zero-sized Guest unit struct, but in the host we provide a
// Host and Weak<Host> for Env. Typically the value is an RawVal or TaggedVal,
// however it can be any type.
#[derive(Clone)]
pub struct EnvVal<E: Env, V> {
    pub env: E,
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

impl<E: Env, T: TagType> EnvVal<E, TaggedVal<T>> {
    // These fns let callers disambiguate by name when they call as_ref() in a
    // context that might want &TaggedVal<T> or &RawRef; it saves them writing
    // <_ as AsRef<RawVal>>::as_ref(foo)

    pub fn as_raw(&self) -> &RawVal {
        &self.val.0
    }
    pub fn to_raw(&self) -> RawVal {
        self.val.0
    }
    pub fn as_tagged(&self) -> &TaggedVal<T> {
        &self.val
    }
    pub fn to_tagged(&self) -> TaggedVal<T> {
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

impl<E: Env, T: TagType> AsRef<TaggedVal<T>> for EnvVal<E, TaggedVal<T>> {
    fn as_ref(&self) -> &TaggedVal<T> {
        &self.val
    }
}

impl<E: Env, T: TagType> AsMut<TaggedVal<T>> for EnvVal<E, TaggedVal<T>> {
    fn as_mut(&mut self) -> &mut TaggedVal<T> {
        &mut self.val
    }
}

impl<E: Env, T: TagType> From<EnvVal<E, TaggedVal<T>>> for EnvVal<E, RawVal> {
    fn from(ev: EnvVal<E, TaggedVal<T>>) -> Self {
        EnvVal {
            env: ev.env,
            val: ev.val.to_raw(),
        }
    }
}

pub trait IntoEnvVal<E: Env, V>: Sized {
    fn into_env_val(self, env: &E) -> EnvVal<E, V>;
}

pub trait IntoVal<E: Env, V> {
    fn into_val(self, env: &E) -> V;
}

impl<E: Env, V, T> IntoVal<E, V> for T
where
    T: IntoEnvVal<E, V>,
{
    fn into_val(self, env: &E) -> V {
        Self::into_env_val(self, env).val
    }
}

pub trait TryIntoEnvVal<E: Env, V>: Sized {
    type Error;
    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, V>, Self::Error>;
}

impl<E: Env, F, T> TryIntoEnvVal<E, T> for F
where
    EnvVal<E, F>: TryInto<T>,
{
    type Error = <EnvVal<E, F> as TryInto<T>>::Error;

    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, T>, Self::Error> {
        let ev: T = EnvVal {
            env: env.clone(),
            val: self,
        }
        .try_into()?;
        Ok(EnvVal {
            env: env.clone(),
            val: ev,
        })
    }
}

pub trait TryIntoVal<E: Env, V> {
    type Error;
    fn try_into_val(self, env: &E) -> Result<V, Self::Error>;
}

impl<E: Env, V, T> TryIntoVal<E, V> for T
where
    T: TryIntoEnvVal<E, V>,
{
    type Error = T::Error;
    fn try_into_val(self, env: &E) -> Result<V, Self::Error> {
        Ok(Self::try_into_env_val(self, env)?.val)
    }
}

pub trait TryFromVal<E: Env, V>: Sized {
    type Error;
    fn try_from_val(env: &E, v: V) -> Result<Self, Self::Error>;
}

impl<E: Env, V, T> TryFromVal<E, V> for T
where
    T: TryFrom<EnvVal<E, V>>,
{
    type Error = T::Error;
    fn try_from_val(env: &E, v: V) -> Result<Self, Self::Error> {
        Self::try_from(EnvVal {
            env: env.clone(),
            val: v,
        })
    }
}

impl<E: Env, V, I: Into<EnvVal<E, V>>> IntoEnvVal<E, V> for I {
    fn into_env_val(self, env: &E) -> EnvVal<E, V> {
        let ev = self.into();
        ev.env.check_same_env(env);
        ev
    }
}

impl<E: Env> From<EnvVal<E, RawVal>> for RawVal {
    fn from(ev: EnvVal<E, RawVal>) -> Self {
        ev.val
    }
}

impl<E: Env> EnvVal<E, RawVal> {
    fn log_err_convert<T>(self) {
        self.env().log_static_fmt_val_static_str(
            "can't convert {} to {}",
            self.to_raw(),
            core::any::type_name::<T>(),
        );
    }
}

impl<E: Env, T: TagType> TryFrom<EnvVal<E, RawVal>> for TaggedVal<T> {
    type Error = ConversionError;

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        ev.to_raw().try_into().map_err(|err| {
            ev.log_err_convert::<Self>();
            err
        })
    }
}

impl<E: Env, T: TagType> TryFrom<EnvVal<E, RawVal>> for EnvVal<E, TaggedVal<T>> {
    type Error = ConversionError;

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        let tv: TaggedVal<T> = ev.to_raw().try_into().map_err(|err| {
            ev.clone().log_err_convert::<Self>();
            err
        })?;
        Ok(tv.in_env(ev.env()))
    }
}

impl<E: Env, T: TagType> IntoEnvVal<E, RawVal> for TaggedVal<T> {
    fn into_env_val(self, env: &E) -> EnvVal<E, RawVal> {
        EnvVal {
            env: env.clone(),
            val: self.to_raw(),
        }
    }
}

impl<E: Env, T: TagType> From<EnvVal<E, TaggedVal<T>>> for RawVal {
    fn from(ev: EnvVal<E, TaggedVal<T>>) -> Self {
        ev.val.0
    }
}

impl<E: Env, T: TagType> From<EnvVal<E, TaggedVal<T>>> for TaggedVal<T> {
    fn from(ev: EnvVal<E, TaggedVal<T>>) -> Self {
        ev.val
    }
}

impl<E: Env, T: TagType> IntoEnvVal<E, TaggedVal<T>> for TaggedVal<T> {
    fn into_env_val(self, env: &E) -> EnvVal<E, TaggedVal<T>> {
        EnvVal {
            env: env.clone(),
            val: self,
        }
    }
}

impl<E: Env> TryFrom<EnvVal<E, RawVal>> for i64 {
    type Error = ConversionError;

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        if ev.val.is_u63() {
            Ok(unsafe { ev.val.unchecked_as_u63() })
        } else if Object::val_is_obj_type(ev.val, ScObjectType::I64) {
            let obj = unsafe { Object::unchecked_from_val(ev.val) };
            Ok(ev.env.obj_to_i64(obj))
        } else {
            ev.log_err_convert::<i64>();
            Err(ConversionError)
        }
    }
}

impl<E: Env> IntoEnvVal<E, RawVal> for i64 {
    fn into_env_val(self, env: &E) -> EnvVal<E, RawVal> {
        let val = if self >= 0 {
            unsafe { RawVal::unchecked_from_u63(self) }
        } else {
            env.obj_from_i64(self).to_raw()
        };
        EnvVal {
            env: env.clone(),
            val,
        }
    }
}

impl<E: Env> TryFrom<EnvVal<E, RawVal>> for u64 {
    type Error = ConversionError;

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        if Object::val_is_obj_type(ev.val, ScObjectType::U64) {
            let obj = unsafe { Object::unchecked_from_val(ev.val) };
            Ok(ev.env.obj_to_u64(obj))
        } else {
            ev.log_err_convert::<u64>();
            Err(ConversionError)
        }
    }
}

impl<E: Env> IntoEnvVal<E, RawVal> for u64 {
    fn into_env_val(self, env: &E) -> EnvVal<E, RawVal> {
        let env = env.clone();
        EnvVal {
            val: env.obj_from_u64(self).to_raw(),
            env,
        }
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFrom<EnvVal<E, RawVal>> for ScVal
where
    ScObject: TryFrom<EnvVal<E, Object>>,
{
    type Error = ConversionError;

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        let ev_clone = ev.clone();
        let env = ev_clone.env;
        let val = ev_clone.val;
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
                    } else if tag_static.is_type(ScStatic::LedgerKeyContractCodeWasm) {
                        Ok(ScVal::Static(ScStatic::LedgerKeyContractCodeWasm))
                    } else {
                        ev.log_err_convert::<Self>();
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
impl<E: Env> TryIntoEnvVal<E, RawVal> for &ScVal
where
    for<'a> &'a ScObject: TryIntoVal<E, Object>,
{
    type Error = ConversionError;
    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, RawVal>, Self::Error> {
        let val = match self {
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
        };
        Ok(EnvVal {
            env: env.clone(),
            val,
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

#[cfg(feature = "std")]
impl<E: Env> TryFrom<EnvVal<E, ScVal>> for () {
    type Error = ();
    fn try_from(ev: EnvVal<E, ScVal>) -> Result<Self, Self::Error> {
        if let ScVal::Static(ScStatic::Void) = ev.val {
            Ok(())
        } else {
            Err(())
        }
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFrom<EnvVal<E, ScVal>> for bool {
    type Error = ();
    fn try_from(ev: EnvVal<E, ScVal>) -> Result<Self, Self::Error> {
        match ev.val {
            ScVal::Static(ScStatic::True) => Ok(true),
            ScVal::Static(ScStatic::False) => Ok(false),
            _ => Err(()),
        }
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFrom<EnvVal<E, ScVal>> for u32 {
    type Error = ();
    fn try_from(ev: EnvVal<E, ScVal>) -> Result<Self, Self::Error> {
        if let ScVal::U32(x) = ev.val {
            Ok(x)
        } else {
            Err(())
        }
    }
}
