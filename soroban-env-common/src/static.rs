use stellar_xdr::ScStatic;

use crate::{
    decl_tagged_val_wrapper_methods, decl_wrapper_direct_abi_support, Env, EnvVal, RawVal, Tag,
};

/// Wrapper for a [RawVal] that is tagged with [Tag::Static], interpreting the
/// [RawVal]'s body as a 32-bit value from a reserved set of "static" values
/// corresponding to the enumerated cases of [ScStatic].
#[repr(C)]
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Static(u32);

decl_wrapper_direct_abi_support!(Static, u32, I32, i32);
decl_tagged_val_wrapper_methods!(Static);

impl From<Static> for RawVal {
    fn from(st: Static) -> Self {
        unsafe { RawVal::from_lo32_and_tag(st.0, Tag::Static) }
    }
}

impl crate::RawValConvertible for Static {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::Static)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        Static(v.get_lo32())
    }
}

impl Static {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatic. Instead we provide an "is_type" to check any specific
    // bit-pattern.

    #[inline(always)]
    pub const fn is_type(&self, ty: ScStatic) -> bool {
        self.0 == ty as u32
    }
}
