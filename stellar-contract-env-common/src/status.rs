use crate::{RawVal, Tag, TagStatus, TaggedVal};
use core::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    marker::PhantomData,
};
use stellar_xdr::ScStatusType;

pub type Status = TaggedVal<TagStatus>;

pub const UNKNOWN_ERROR: Status = TaggedVal(
    unsafe { RawVal::from_major_minor_and_tag(0, ScStatusType::UnknownError as u32, Tag::Status) },
    PhantomData,
);
pub const OK: Status = TaggedVal(
    unsafe { RawVal::from_major_minor_and_tag(0, ScStatusType::Ok as u32, Tag::Status) },
    PhantomData,
);

impl Hash for Status {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().get_payload().hash(state);
    }
}

impl PartialEq for Status {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().get_payload() == other.as_ref().get_payload()
    }
}

impl Eq for Status {}

impl PartialOrd for Status {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Status {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        let self_tup = (self.as_ref().get_minor(), self.as_ref().get_major());
        let other_tup = (other.as_ref().get_minor(), other.as_ref().get_major());
        self_tup.cmp(&other_tup)
    }
}

impl Status {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatusType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    #[inline(always)]
    pub const fn is_type(&self, ty: ScStatusType) -> bool {
        self.0.has_minor(ty as u32)
    }

    #[inline(always)]
    pub const fn get_code(&self) -> u32 {
        self.0.get_major()
    }

    #[inline(always)]
    pub const fn is_ok(&self) -> bool {
        self.is_type(ScStatusType::Ok)
    }

    #[inline(always)]
    pub const fn from_type_and_code(ty: ScStatusType, code: u32) -> Status {
        // Unfortunately we can't use from_major_minor_and_tag_type here because
        // it's not const, and making it const requires nightly.
        unsafe {
            Self(
                RawVal::from_major_minor_and_tag(code, ty as u32, Tag::Status),
                PhantomData,
            )
        }
    }
}
