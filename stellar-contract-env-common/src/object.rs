use stellar_xdr::{ScObject, ScVal};

use crate::{
    tagged_val::{TagObject, TaggedVal},
    xdr::ScObjectType,
    Env, EnvVal, RawVal, RawValConvertible, Tag, TryConvert, TryIntoEnvVal,
};
use core::fmt::Debug;

#[derive(Copy, Clone)]
pub struct Object(TaggedVal<TagObject>);

impl Debug for Object {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let object_type_res: Result<ScObjectType, _> =
            (self.as_raw().get_minor() as i32).try_into();
        let object_type_name: &str = match &object_type_res {
            Ok(ty) => ty.name(),
            Err(_) => &"Unknown",
        };
        let index = self.as_raw().get_major();
        write!(f, "Object({}({}))", object_type_name, index)
    }
}

impl Object {
    pub fn in_env<E: Env>(self, env: &E) -> EnvVal<E, Object> {
        EnvVal {
            env: env.clone(),
            val: self,
        }
    }

    pub const fn as_raw(&self) -> &RawVal {
        &self.0 .0
    }

    pub const fn to_raw(&self) -> RawVal {
        self.0 .0
    }

    pub const fn as_tagged(&self) -> &TaggedVal<TagObject> {
        &self.0
    }

    pub const fn to_tagged(&self) -> TaggedVal<TagObject> {
        self.0
    }

    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatusType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    #[inline(always)]
    pub const fn is_obj_type(&self, ty: ScObjectType) -> bool {
        self.as_raw().has_minor(ty as u32)
    }

    #[inline(always)]
    pub const fn get_handle(&self) -> u32 {
        self.as_raw().get_major()
    }

    #[inline(always)]
    pub fn val_is_obj_type(v: RawVal, ty: ScObjectType) -> bool {
        v.has_tag(Tag::Object) && v.has_minor(ty as u32)
    }

    #[inline(always)]
    pub fn from_type_and_handle(ty: ScObjectType, handle: u32) -> Self {
        Self(unsafe { TaggedVal::from_major_minor_and_tag_type(handle, ty as u32) })
    }
}

impl AsRef<TaggedVal<TagObject>> for Object {
    fn as_ref(&self) -> &TaggedVal<TagObject> {
        self.as_tagged()
    }
}

impl AsRef<RawVal> for Object {
    fn as_ref(&self) -> &RawVal {
        self.as_raw()
    }
}

impl AsMut<RawVal> for Object {
    fn as_mut(&mut self) -> &mut RawVal {
        self.0.as_mut()
    }
}

impl From<TaggedVal<TagObject>> for Object {
    fn from(tv: TaggedVal<TagObject>) -> Self {
        Object(tv)
    }
}

impl From<Object> for TaggedVal<TagObject> {
    fn from(b: Object) -> Self {
        b.to_tagged()
    }
}

impl From<Object> for RawVal {
    fn from(b: Object) -> Self {
        b.to_raw()
    }
}

impl RawValConvertible for Object {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        <TaggedVal<TagObject> as RawValConvertible>::is_val_type(v)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        Object(<TaggedVal<TagObject> as RawValConvertible>::unchecked_from_val(v))
    }
}

#[cfg(feature = "vm")]
impl wasmi::FromValue for Object {
    fn from_value(val: wasmi::RuntimeValue) -> Option<Self> {
        let maybe: Option<TaggedVal<TagObject>> = val.try_into();
        maybe.map(|x| Self(x))
    }
}

#[cfg(feature = "vm")]
impl From<Object> for wasmi::RuntimeValue {
    fn from(v: Object) -> Self {
        wasmi::RuntimeValue::I64(v.as_raw().get_payload() as i64)
    }
}

impl<E> TryFrom<EnvVal<E, Object>> for ScObject
where
    E: Env + TryConvert<Object, ScObject>,
{
    type Error = E::Error;
    fn try_from(ev: EnvVal<E, Object>) -> Result<Self, Self::Error> {
        ev.env.convert(ev.val)
    }
}

impl<E> TryFrom<&EnvVal<E, Object>> for ScObject
where
    E: Env + TryConvert<Object, ScObject>,
{
    type Error = E::Error;
    fn try_from(ev: &EnvVal<E, Object>) -> Result<Self, Self::Error> {
        ev.env.convert(ev.val)
    }
}

impl<'a, E> TryIntoEnvVal<E, Object> for &'a ScObject
where
    E: Env + TryConvert<&'a ScObject, Object>,
{
    type Error = E::Error;
    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, Object>, Self::Error> {
        Ok(EnvVal {
            val: env.convert(self)?,
            env: env.clone(),
        })
    }
}

impl<E> TryIntoEnvVal<E, Object> for ScObject
where
    E: Env + TryConvert<ScObject, Object>,
{
    type Error = E::Error;
    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, Object>, Self::Error> {
        Ok(EnvVal {
            val: env.convert(self)?,
            env: env.clone(),
        })
    }
}

impl<E> TryFrom<EnvVal<E, Object>> for ScVal
where
    E: Env + TryConvert<Object, ScObject>,
{
    type Error = E::Error;
    fn try_from(ev: EnvVal<E, Object>) -> Result<Self, Self::Error> {
        (&ev).try_into()
    }
}

impl<E> TryFrom<&EnvVal<E, Object>> for ScVal
where
    E: Env + TryConvert<Object, ScObject>,
{
    type Error = E::Error;
    fn try_from(ev: &EnvVal<E, Object>) -> Result<Self, Self::Error> {
        Ok(ScVal::Object(Some(ev.env.convert(ev.val)?)))
    }
}

impl<'a, E> TryIntoEnvVal<E, Object> for &'a ScVal
where
    E: Env + TryConvert<&'a ScObject, Object>,
{
    type Error = E::Error;
    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, Object>, Self::Error> {
        if let ScVal::Object(Some(o)) = self {
            o.try_into_env_val(env)
        } else {
            todo!()
        }
    }
}

impl<E> TryIntoEnvVal<E, Object> for ScVal
where
    E: Env + TryConvert<ScObject, Object>,
{
    type Error = E::Error;
    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, Object>, Self::Error> {
        if let ScVal::Object(Some(o)) = self {
            o.try_into_env_val(env)
        } else {
            todo!()
        }
    }
}
