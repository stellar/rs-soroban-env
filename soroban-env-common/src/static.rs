use stellar_xdr::ScStatic;

use crate::{decl_tagged_val_wrapper_methods, RawVal, Tag};

/// Wrapper for a [RawVal] that is tagged with [Tag::Static], interpreting the
/// [RawVal]'s body as a 32-bit value from a reserved set of "static" values
/// corresponding to the enumerated cases of [ScStatic].
#[derive(Copy, Clone)]
pub struct Static(RawVal);

decl_tagged_val_wrapper_methods!(Static);

impl From<()> for Static {
    fn from(_: ()) -> Self {
        Self(RawVal::VOID)
    }
}

impl From<bool> for Static {
    fn from(a: bool) -> Self {
        Self(RawVal::from_bool(a))
    }
}

impl Static {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatic. Instead we provide an "is_type" to check any specific
    // bit-pattern.

    #[inline(always)]
    pub const fn is_type(&self, ty: ScStatic) -> bool {
        self.as_raw().has_minor(ty as u32)
    }
}
