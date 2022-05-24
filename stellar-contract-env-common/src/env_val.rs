use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use stellar_xdr::{ScStatic, ScStatus, ScStatusType, ScVal};

use crate::{BitSet, Object, Status, Symbol, Tag, TagType, TaggedVal, Val};

#[cfg(feature = "std")]
use crate::{BitSetError, ObjectXdrConverter, SymbolError};

use super::{
    raw_val::{RawVal, RawValConvertible},
    Env,
};
use core::{cmp::Ordering, fmt::Debug};

// EnvVal is a RawVal or TaggedVal coupled to a specific instance of Env. In the
// guest we will use this with a zero-sized Guest unit struct, but in the host
// we provide a Host and Weak<Host> for Env.
#[derive(Clone)]
pub struct EnvVal<E: Env, V: Val> {
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
            val: ev.val.as_ref().clone(),
        }
    }
}

pub trait IntoEnvVal<E: Env, V: Val>: Sized {
    fn into_env_val(self, env: &E) -> EnvVal<E, V>;
}

pub trait IntoVal<E: Env, V: Val>: IntoEnvVal<E, V> {
    fn into_val(self, env: &E) -> V {
        Self::into_env_val(self, env).val
    }
}

impl<E: Env, V: Val, T> IntoVal<E, V> for T where T: IntoEnvVal<E, V> {}

pub trait TryIntoEnvVal<E: Env, V: Val>: Sized {
    type Error;
    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, V>, Self::Error>;
}

pub trait TryIntoVal<E: Env, V: Val>: TryIntoEnvVal<E, V> {
    fn try_into_val(self, env: &E) -> Result<V, Self::Error> {
        Ok(Self::try_into_env_val(self, env)?.val)
    }
}

impl<E: Env, V: Val, T> TryIntoVal<E, V> for T where T: TryIntoEnvVal<E, V> {}

// impl<E: Env, V: Val, T> TryIntoEnvVal<E, V> for T
// where
//     T: IntoEnvVal<E, V>,
// {
//     type Error = ();

//     fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, V>, Self::Error> {
//         Ok(self.into_env_val(env))
//     }
// }

pub trait TryFromVal<E: Env, V: Val>: Sized + TryFrom<EnvVal<E, V>> {
    fn try_from_val(env: &E, v: V) -> Result<Self, Self::Error> {
        Self::try_from(EnvVal {
            env: env.clone(),
            val: v,
        })
    }
}

impl<E: Env, V: Val, T> TryFromVal<E, V> for T where T: Sized + TryFrom<EnvVal<E, V>> {}

impl<E: Env, V: Val, I: Into<EnvVal<E, V>>> IntoEnvVal<E, V> for I {
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

impl<E: Env, T: TagType> TryFrom<EnvVal<E, RawVal>> for TaggedVal<T> {
    type Error = ();

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        Ok(ev.to_raw().try_into()?)
    }
}

impl<E: Env, T: TagType> TryFrom<EnvVal<E, RawVal>> for EnvVal<E, TaggedVal<T>> {
    type Error = ();

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        let tv: TaggedVal<T> = ev.to_raw().try_into()?;
        Ok(tv.in_env(ev.env()))
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
            val: self.clone(),
        }
    }
}

impl<E: Env> TryFrom<EnvVal<E, RawVal>> for i64 {
    type Error = ();

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        if ev.val.is_positive_i64() {
            Ok(unsafe { ev.val.unchecked_as_positive_i64() })
        } else if Object::val_is_obj_type(ev.val, ScObjectType::I64) {
            let obj = unsafe { Object::unchecked_from_val(ev.val) };
            Ok(ev.env.obj_to_i64(obj))
        } else {
            Err(())
        }
    }
}

impl<E: Env> IntoEnvVal<E, RawVal> for i64 {
    fn into_env_val(self, env: &E) -> EnvVal<E, RawVal> {
        let val = if self >= 0 {
            unsafe { RawVal::unchecked_from_positive_i64(self) }
        } else {
            env.obj_from_i64(self).as_ref().clone()
        };
        EnvVal {
            env: env.clone(),
            val,
        }
    }
}

impl<E: Env> TryFrom<EnvVal<E, RawVal>> for u64 {
    type Error = ();

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        if Object::val_is_obj_type(ev.val, ScObjectType::U64) {
            let obj = unsafe { Object::unchecked_from_val(ev.val) };
            Ok(ev.env.obj_to_u64(obj))
        } else {
            Err(())
        }
    }
}

impl<E: Env> IntoEnvVal<E, RawVal> for u64 {
    fn into_env_val(self, env: &E) -> EnvVal<E, RawVal> {
        let env = env.clone();
        EnvVal {
            val: env.obj_from_u64(self).as_ref().clone(),
            env,
        }
    }
}

#[cfg(feature = "std")]
impl<E: Env + ObjectXdrConverter> TryFrom<EnvVal<E, RawVal>> for ScVal {
    type Error = ();

    fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
        let env = ev.env;
        let val = ev.val;
        if val.is_positive_i64() {
            Ok(ScVal::U63(unsafe { val.unchecked_as_positive_i64() } as u64))
        } else {
            match val.get_tag() {
                Tag::U32 => Ok(ScVal::U32(unsafe {
                    <u32 as RawValConvertible>::unchecked_from_val(val)
                })),
                Tag::I32 => Ok(ScVal::I32(unsafe {
                    <i32 as RawValConvertible>::unchecked_from_val(val)
                })),
                Tag::Static => {
                    if let Some(b) = <bool as RawValConvertible>::try_convert(val) {
                        if b {
                            Ok(ScVal::Static(ScStatic::True))
                        } else {
                            Ok(ScVal::Static(ScStatic::False))
                        }
                    } else if <() as RawValConvertible>::is_val_type(val) {
                        Ok(ScVal::Static(ScStatic::Void))
                    } else {
                        Err(())
                    }
                }
                Tag::Object => unsafe {
                    let ob = <Object as RawValConvertible>::unchecked_from_val(val);
                    let scob = env.from_xdr_obj(ob)?;
                    Ok(ScVal::Object(Some(Box::new(scob))))
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
                    if status.is_ok() {
                        Ok(ScVal::Status(ScStatus::Ok))
                    } else if status.is_type(ScStatusType::UnknownError) {
                        Ok(ScVal::Status(ScStatus::UnknownError(status.get_code())))
                    } else {
                        Err(())
                    }
                }
                Tag::Reserved => Err(()),
            }
        }
    }
}

#[cfg(feature = "std")]
pub enum TryIntoEnvValScValError {
    Error,
    BitSetError(BitSetError),
    SymbolError(SymbolError),
}

#[cfg(feature = "std")]
impl From<()> for TryIntoEnvValScValError {
    fn from(_: ()) -> Self {
        Self::Error
    }
}

#[cfg(feature = "std")]
impl From<BitSetError> for TryIntoEnvValScValError {
    fn from(e: BitSetError) -> Self {
        Self::BitSetError(e)
    }
}

#[cfg(feature = "std")]
impl From<SymbolError> for TryIntoEnvValScValError {
    fn from(e: SymbolError) -> Self {
        Self::SymbolError(e)
    }
}

#[cfg(feature = "std")]
impl<E: Env + ObjectXdrConverter> TryIntoEnvVal<E, RawVal> for &ScVal {
    type Error = TryIntoEnvValScValError;

    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, RawVal>, Self::Error> {
        let val = match self {
            ScVal::U63(u) => {
                if *u <= (i64::MAX as u64) {
                    unsafe { RawVal::unchecked_from_positive_i64(*u as i64) }
                } else {
                    return Err(Self::Error::Error);
                }
            }
            ScVal::U32(u) => (*u).into(),
            ScVal::I32(i) => (*i).into(),
            ScVal::Static(ScStatic::Void) => RawVal::from_void(),
            ScVal::Static(ScStatic::True) => RawVal::from_bool(true),
            ScVal::Static(ScStatic::False) => RawVal::from_bool(false),
            ScVal::Object(None) => return Err(Self::Error::Error),
            ScVal::Object(Some(ob)) => env.to_xdr_obj(&*ob)?.to_raw(),
            ScVal::Symbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(Self::Error::Error),
                };
                Symbol::try_from_str(ss)?.into()
            }
            ScVal::Bitset(i) => BitSet::try_from_u64(*i)?.into(),
            ScVal::Status(st) => {
                let status = match st {
                    ScStatus::Ok => Status::from_type_and_code(ScStatusType::Ok, 0),
                    ScStatus::UnknownError(e) => {
                        Status::from_type_and_code(ScStatusType::UnknownError, *e)
                    }
                };
                status.into()
            }
        };
        Ok(EnvVal {
            env: env.clone(),
            val,
        })
    }
}

impl<E: Env + Debug, V: Val> Debug for EnvVal<E, V> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("EnvVal")
            .field("env", &self.env)
            .field("val", &self.as_ref())
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
