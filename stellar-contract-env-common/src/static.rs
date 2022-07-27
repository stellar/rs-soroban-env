use stellar_xdr::ScStatic;

use crate::{decl_tagged_val_wrapper, impl_wrapper_from, Env, EnvVal, RawVal, Tag};

decl_tagged_val_wrapper!(Static);

impl_wrapper_from!((), Static);
impl_wrapper_from!(bool, Static);

impl Static {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatic. Instead we provide an "is_type" to check any specific
    // bit-pattern.

    #[inline(always)]
    pub const fn is_type(&self, ty: ScStatic) -> bool {
        self.as_raw().has_minor(ty as u32)
    }
}
