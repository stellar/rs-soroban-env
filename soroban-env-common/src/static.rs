use crate::{impl_wrapper_common, impl_wrapper_from, ConversionError, Env, RawVal, Tag};
use core::cmp::{Eq, Ord, PartialEq, PartialOrd};
use stellar_xdr::ScStatic;

/// Wrapper for a [RawVal] that is tagged with [Tag::Static], interpreting the
/// [RawVal]'s body as a 32-bit value from a reserved set of "static" values
/// corresponding to the enumerated cases of [ScStatic].
#[derive(Copy, Clone)]
pub struct Static(RawVal);

impl_wrapper_common!(Static);

impl_wrapper_from!((), Static);
impl_wrapper_from!(bool, Static);

impl TryFrom<Static> for bool {
    type Error = ConversionError;

    fn try_from(value: Static) -> Result<Self, Self::Error> {
        if value.is_type(ScStatic::True) {
            Ok(true)
        } else if value.is_type(ScStatic::False) {
            Ok(false)
        } else {
            Err(ConversionError)
        }
    }
}

impl TryFrom<Static> for () {
    type Error = ConversionError;

    fn try_from(value: Static) -> Result<Self, Self::Error> {
        if value.is_type(ScStatic::Void) {
            Ok(())
        } else {
            Err(ConversionError)
        }
    }
}

impl Eq for Static {}

impl PartialEq for Static {
    fn eq(&self, other: &Self) -> bool {
        self.as_raw().get_payload() == other.as_raw().get_payload()
    }
}

impl Ord for Static {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.as_raw()
            .get_payload()
            .cmp(&other.as_raw().get_payload())
    }
}
impl PartialOrd for Static {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
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
