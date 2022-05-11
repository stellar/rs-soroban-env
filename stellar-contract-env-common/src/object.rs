use super::{xdr::ScObjectType, RawVal, Tag};
use crate::tagged_val::{TagObject, TaggedVal};

pub type Object = TaggedVal<TagObject>;

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
