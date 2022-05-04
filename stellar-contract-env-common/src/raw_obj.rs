use super::{xdr::ScObjectType, Tag, Val, ValType};

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct RawObj(Val);

impl ValType for RawObj {
    #[inline(always)]
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::Object)
    }

    #[inline(always)]
    unsafe fn unchecked_from_val(v: Val) -> Self {
        RawObj(v)
    }
}

impl From<RawObj> for Val {
    #[inline(always)]
    fn from(s: RawObj) -> Self {
        s.0
    }
}

impl AsRef<Val> for RawObj {
    #[inline(always)]
    fn as_ref(&self) -> &Val {
        &self.0
    }
}

impl AsMut<Val> for RawObj {
    #[inline(always)]
    fn as_mut(&mut self) -> &mut Val {
        &mut self.0
    }
}

impl RawObj {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatusType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    #[inline(always)]
    pub const fn is_type(&self, ty: ScObjectType) -> bool {
        self.0.has_minor(ty as u32)
    }

    #[inline(always)]
    pub const fn get_handle(&self) -> u32 {
        self.0.get_major()
    }

    #[inline(always)]
    pub fn val_is_obj_type(v: Val, ty: ScObjectType) -> bool {
        v.has_tag(Tag::Object) && v.has_minor(ty as u32)
    }

    #[inline(always)]
    pub const fn from_type_and_code(ty: ScObjectType, code: u32) -> RawObj {
        RawObj(unsafe { Val::from_major_minor_and_tag(code, ty as u32, Tag::Object) })
    }
}
