use stellar_xdr::{ScObject, ScVal};

use crate::{
    tagged_val::{TagObject, TaggedVal},
    xdr::ScObjectType,
    Env, EnvVal, RawVal, Tag, TryConvert, TryIntoVal,
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

impl<'a, E> TryIntoVal<E, Object> for &'a ScObject
where
    E: Env + TryConvert<&'a ScObject, Object>,
{
    type Error = E::Error;
    fn try_into_val(self, env: &E) -> Result<Object, Self::Error> {
        env.convert(self)
    }
}

impl<E> TryIntoVal<E, Object> for ScObject
where
    E: Env + TryConvert<ScObject, Object>,
{
    type Error = E::Error;
    fn try_into_val(self, env: &E) -> Result<Object, Self::Error> {
        env.convert(self)
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

impl<'a, E> TryIntoVal<E, Object> for &'a ScVal
where
    E: Env + TryConvert<&'a ScObject, Object>,
{
    type Error = E::Error;
    fn try_into_val(self, env: &E) -> Result<Object, Self::Error> {
        if let ScVal::Object(Some(o)) = self {
            o.try_into_val(env)
        } else {
            todo!()
        }
    }
}

impl<E> TryIntoVal<E, Object> for ScVal
where
    E: Env + TryConvert<ScObject, Object>,
{
    type Error = E::Error;
    fn try_into_val(self, env: &E) -> Result<Object, Self::Error> {
        if let ScVal::Object(Some(o)) = self {
            o.try_into_val(env)
        } else {
            todo!()
        }
    }
}
