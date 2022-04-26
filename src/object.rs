use super::val::{Tag, Val, ValType};
use stellar_xdr::ScObjectType;

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Object(Val);

impl ValType for Object {
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::Object)
    }

    unsafe fn unchecked_from_val(v: Val) -> Self {
        Object(v)
    }
}

pub trait ObjType: Into<Val> {
    fn is_obj_type(obj: Object) -> bool;
    unsafe fn unchecked_from_obj(obj: Object) -> Self;
}

impl<OB: ObjType> ValType for OB {
    fn is_val_type(v: Val) -> bool {
        v.is::<Object>() && <Self as ObjType>::is_obj_type(Object(v))
    }

    unsafe fn unchecked_from_val(v: Val) -> Self {
        <Self as ObjType>::unchecked_from_obj(Object(v))
    }
}

impl From<Object> for Val {
    #[inline(always)]
    fn from(obj: Object) -> Self {
        obj.0
    }
}

impl Object {
    #[inline(always)]
    pub fn get_handle(&self) -> u32 {
        self.0.get_major()
    }

    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern
    // into an ScObjectType. Instead we provide an "is_type" below to check
    // any specific bit-pattern.

    pub fn from_type_and_handle(ty: ScObjectType, handle: u32) -> Object {
        let v = unsafe { Val::from_major_minor_and_tag(handle, ty as u32, Tag::Object) };
        Object(v)
    }

    // This is just an optimized 2-checks-in-one check.
    #[inline(always)]
    pub fn val_is_obj_type(v: Val, ty: ScObjectType) -> bool {
        v.is::<Object>() && Object(v).is_type(ty)
    }

    #[inline(always)]
    pub fn is_type(&self, ty: ScObjectType) -> bool {
        self.0.has_minor(ty as u32)
    }
}
