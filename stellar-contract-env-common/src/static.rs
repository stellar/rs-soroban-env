use stellar_xdr::ScStatic;

use crate::{TagStatic, TaggedVal};

pub type Static = TaggedVal<TagStatic>;
impl Static {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatic. Instead we provide an "is_type" to check any specific
    // bit-pattern.

    #[inline(always)]
    pub const fn is_type(&self, ty: ScStatic) -> bool {
        self.0.has_minor(ty as u32)
    }
}
