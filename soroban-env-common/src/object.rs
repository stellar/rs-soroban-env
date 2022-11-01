use crate::{
    decl_tagged_val_wrapper_methods, decl_wrapper_direct_abi_support, Env, EnvVal, RawVal, Tag,
    TryConvert, TryFromVal, TryIntoVal,
};
use core::fmt::Debug;
use stellar_xdr::{ScObject, ScVal, ScObjectType};

/// Wrapper for a [RawVal] that is tagged with [Tag::Object], interpreting the
/// [RawVal]'s body as a pair of a 32-bit object-type code and a 32-bit handle
/// to a host object of the object-type. The object-type codes correspond to the
/// enumerated cases of [ScObject], and the handle values are dynamically
/// assigned by the host as new objects are allocated during execution.
#[repr(C)]
#[derive(Copy, Clone)]
pub struct Object(u64);

decl_wrapper_direct_abi_support!(Object, u64, I64, i64);
decl_tagged_val_wrapper_methods!(Object);

impl From<Object> for RawVal {
    fn from(obj: Object) -> Self {
        unsafe { RawVal::from_lo64_and_tag(obj.0, Tag::Object) }
    }
}

impl crate::RawValConvertible for Object {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::Object)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        Object(v.get_lo64())
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let ty: u32 = self.0 as u32;
        let ty: &'static str = ScObjectType::VARIANTS_STR.get(ty as usize).unwrap_or(&"Unknown");
        write!(f, "Object({}(#{}))", ty, self.get_handle())
    }
}

impl Object {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatusType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    #[inline(always)]
    pub fn is_obj_type(&self, ty: ScObjectType) -> bool {
        (self.0 as u32) == (ty as u32)
    }

    #[inline(always)]
    pub const fn get_handle(&self) -> u32 {
        (self.0 >> 32) as u32
    }

    #[inline(always)]
    pub const fn val_is_obj_type(v: RawVal, ty: ScObjectType) -> bool {
            v.has_tag(Tag::Object) && v.get_lo32() == (ty as u32)
    }

    #[inline(always)]
    pub const fn to_raw(&self) -> RawVal {
        unsafe { RawVal::from_lo64_and_tag(self.0, Tag::Object) }
    }

    #[inline(always)]
    pub fn from_type_and_handle(ty: ScObjectType, handle: u32) -> Self {
        Self(((handle as u64) << 32) | ty as u64)
    }
}

impl<E> TryFromVal<E, Object> for ScObject
where
    E: Env + TryConvert<Object, ScObject>,
{
    type Error = E::Error;
    fn try_from_val(env: &E, val: Object) -> Result<Self, Self::Error> {
        env.convert(val)
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
