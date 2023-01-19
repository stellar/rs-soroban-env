use crate::{
    impl_wrapper_common, xdr::ScObjectType, ConversionError, Convert, Env, MapErrToEnv, RawVal,
    Tag, TryFromVal, TryIntoVal,
};
use core::fmt::Debug;
use stellar_xdr::{ScObject, ScVal};

/// Wrapper for a [RawVal] that is tagged with [Tag::Object], interpreting the
/// [RawVal]'s body as a pair of a 28-bit object-type code and a 32-bit handle
/// to a host object of the object-type. The object-type codes correspond to the
/// enumerated cases of [ScObject], and the handle values are dynamically
/// assigned by the host as new objects are allocated during execution.
#[derive(Copy, Clone)]
pub struct Object(RawVal);

impl_wrapper_common!(Object);

impl Debug for Object {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let object_type_res: Result<ScObjectType, _> =
            (self.as_raw().get_minor() as i32).try_into();
        let object_type_name: &str = match &object_type_res {
            Ok(ty) => ty.name(),
            Err(_) => &"Unknown",
        };
        let index = self.as_raw().get_major();
        write!(f, "Object({}(#{}))", object_type_name, index)
    }
}

impl Object {
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
        unsafe { Self::from_major_minor(handle, ty as u32) }
    }
}

impl<E> TryFromVal<E, Object> for ScObject
where
    E: Env + Convert<Object, ScObject>,
{
    fn try_from_val(env: &E, val: &Object) -> Result<Self, E::Error> {
        env.convert(*val)
    }
}

impl<E> TryFromVal<E, ScObject> for Object
where
    E: Env + for<'a> Convert<&'a ScObject, Object>,
{
    // We want this:
    //
    //   type Error = <E as for<'a> Convert<&'a ScObject, Object>>::Error;
    //
    // But due to lifetime difficulties we will use a simpler error for now.
    // (This is temporary anyways, we'll be moving the errors to EnvBase soon)

    fn try_from_val(env: &E, v: &ScObject) -> Result<Self, E::Error> {
        let scob: &ScObject = v;
        env.convert(scob)
    }
}

impl<E> TryFromVal<E, ScVal> for Object
where
    E: Env + for<'a> Convert<&'a ScObject, Object>,
{
    fn try_from_val(env: &E, v: &ScVal) -> Result<Self, E::Error> {
        if let ScVal::Object(Some(o)) = v {
            o.try_into_val(env)
        } else {
            Err(ConversionError).map_err_to_env(env)
        }
    }
}
