use super::val::{Tag, Val, ValType};
use stellar_xdr::ScStatusType;

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Status(Val);

pub const UNKNOWN_ERROR: Status = Status(unsafe {
    Val::from_major_minor_and_tag(0, ScStatusType::SstUnknownError as u32, Tag::Status)
});
pub const OK: Status =
    Status(unsafe { Val::from_major_minor_and_tag(0, ScStatusType::SstOk as u32, Tag::Status) });

impl ValType for Status {
    #[inline(always)]
    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::Status)
    }

    #[inline(always)]
    unsafe fn unchecked_from_val(v: Val) -> Self {
        Status(v)
    }
}

impl From<Status> for Val {
    #[inline(always)]
    fn from(s: Status) -> Self {
        s.0
    }
}

impl Status {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatusType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    fn is_type(&self, ty: ScStatusType) -> bool {
        self.0.has_minor(ty as u32)
    }
    pub fn is_ok(&self) -> bool {
        self.is_type(ScStatusType::SstOk)
    }
}
