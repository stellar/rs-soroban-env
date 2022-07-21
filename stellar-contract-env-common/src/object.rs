use stellar_xdr::ScObject;

use crate::{
    tagged_val::{TagObject, TaggedVal},
    xdr::ScObjectType,
    Env, EnvVal, RawVal, Tag, TryIntoEnvVal,
};
use core::fmt::Debug;

pub type Object = TaggedVal<TagObject>;

impl Debug for Object {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let object_type_res: Result<ScObjectType, _> = (self.0.get_minor() as i32).try_into();
        let object_type_name: &str = match &object_type_res {
            Ok(ty) => ty.name(),
            Err(_) => &"Unknown",
        };
        let index = self.0.get_major();
        write!(f, "Object({}({}))", object_type_name, index)
    }
}

impl Object {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatusType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    #[inline(always)]
    pub const fn is_obj_type(&self, ty: ScObjectType) -> bool {
        self.0.has_minor(ty as u32)
    }

    #[inline(always)]
    pub const fn get_handle(&self) -> u32 {
        self.0.get_major()
    }

    #[inline(always)]
    pub fn val_is_obj_type(v: RawVal, ty: ScObjectType) -> bool {
        v.has_tag(Tag::Object) && v.has_minor(ty as u32)
    }

    #[inline(always)]
    pub fn from_type_and_handle(ty: ScObjectType, handle: u32) -> Self {
        unsafe { TaggedVal::from_major_minor_and_tag_type(handle, ty as u32) }
    }
}

impl<E: Env + FromObject> TryFrom<EnvVal<E, Object>> for ScObject {
    type Error = E::Error;
    fn try_from(ev: EnvVal<E, Object>) -> Result<Self, Self::Error> {
        ev.env.from_object(ev.val)
    }
}

impl<E: Env + FromObject> TryFrom<&EnvVal<E, Object>> for ScObject {
    type Error = E::Error;
    fn try_from(ev: &EnvVal<E, Object>) -> Result<Self, Self::Error> {
        ev.env.from_object(ev.val)
    }
}

impl<E: Env + ToObject> TryIntoEnvVal<E, Object> for &ScObject {
    type Error = E::Error;
    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, Object>, Self::Error> {
        Ok(EnvVal {
            val: env.to_object(self)?,
            env: env.clone(),
        })
    }
}

impl<E: Env + ToObject> TryIntoEnvVal<E, Object> for ScObject {
    type Error = E::Error;
    fn try_into_env_val(self, env: &E) -> Result<EnvVal<E, Object>, Self::Error> {
        (&self).try_into_env_val(env)
    }
}

pub trait ToObject {
    type Error;
    fn to_object(&self, ob: &ScObject) -> Result<Object, Self::Error>;
}

pub trait FromObject {
    type Error;
    fn from_object(&self, ob: Object) -> Result<ScObject, Self::Error>;
}
