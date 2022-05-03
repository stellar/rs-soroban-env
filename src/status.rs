use super::val::{Tag, Val, ValType};
use std::hash::Hash;
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

impl Hash for Status {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.get_payload().hash(state);
    }
}

impl PartialEq for Status {
    fn eq(&self, other: &Self) -> bool {
        self.0.get_payload() == other.0.get_payload()
    }
}

impl Eq for Status {}

impl PartialOrd for Status {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Status {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let self_tup = (self.0.get_minor(), self.0.get_major());
        let other_tup = (other.0.get_minor(), other.0.get_major());
        self_tup.cmp(&other_tup)
    }
}

impl Status {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatusType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    pub const fn is_type(&self, ty: ScStatusType) -> bool {
        self.0.has_minor(ty as u32)
    }
    pub const fn get_code(&self) -> u32 {
        self.0.get_major()
    }
    pub const fn is_ok(&self) -> bool {
        self.is_type(ScStatusType::SstOk)
    }

    pub const fn from_type_and_code(ty: ScStatusType, code: u32) -> Status {
        Status(unsafe { Val::from_major_minor_and_tag(code, ty as u32, Tag::Status) })
    }
}
