use stellar_xdr::ScStatic;

use crate::{RawVal, Tag, TagStatic, TaggedVal};

pub type Static = TaggedVal<TagStatic>;
impl Static {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatic. Instead we provide an "is_type" to check any specific
    // bit-pattern.

    pub const fn is_void(rv: &RawVal) -> bool {
        rv.has_tag(Tag::Static) && rv.has_minor(ScStatic::Void as u32)
    }

    pub const fn is_true(rv: &RawVal) -> bool {
        rv.has_tag(Tag::Static) && rv.has_minor(ScStatic::True as u32)
    }

    pub const fn is_false(rv: &RawVal) -> bool {
        rv.has_tag(Tag::Static) && rv.has_minor(ScStatic::False as u32)
    }

    #[inline(always)]
    pub const fn is_type(&self, ty: ScStatic) -> bool {
        self.0.has_minor(ty as u32)
    }
}

#[cfg(test)]
mod test {
    use crate::RawVal;

    use super::Static;

    #[test]
    fn test_void() {
        let rv: RawVal = ().into();
        assert!(Static::is_void(&rv));
        assert!(!Static::is_true(&rv));
        assert!(!Static::is_false(&rv));
    }

    #[test]
    fn test_true() {
        let rv: RawVal = true.into();
        assert!(!Static::is_void(&rv));
        assert!(Static::is_true(&rv));
        assert!(!Static::is_false(&rv));
    }

    #[test]
    fn test_false() {
        let rv: RawVal = false.into();
        assert!(!Static::is_void(&rv));
        assert!(!Static::is_true(&rv));
        assert!(Static::is_false(&rv));
    }
}
