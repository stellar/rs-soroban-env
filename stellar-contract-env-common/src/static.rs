use stellar_xdr::ScStatic;

use crate::{Env, EnvVal, RawVal, RawValConvertible, TagStatic, TaggedVal};

#[derive(Copy, Clone)]
pub struct Static(TaggedVal<TagStatic>);

impl Static {
    pub fn in_env<E: Env>(self, env: &E) -> EnvVal<E, Static> {
        EnvVal {
            env: env.clone(),
            val: self,
        }
    }
    pub const fn as_raw(&self) -> &RawVal {
        &self.0 .0
    }

    pub const fn to_raw(&self) -> RawVal {
        self.0 .0
    }

    pub const fn as_tagged(&self) -> &TaggedVal<TagStatic> {
        &self.0
    }

    pub const fn to_tagged(&self) -> TaggedVal<TagStatic> {
        self.0
    }

    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatic. Instead we provide an "is_type" to check any specific
    // bit-pattern.

    #[inline(always)]
    pub const fn is_type(&self, ty: ScStatic) -> bool {
        self.as_raw().has_minor(ty as u32)
    }
}

impl AsRef<TaggedVal<TagStatic>> for Static {
    fn as_ref(&self) -> &TaggedVal<TagStatic> {
        self.as_tagged()
    }
}

impl AsRef<RawVal> for Static {
    fn as_ref(&self) -> &RawVal {
        self.as_raw()
    }
}

impl AsMut<RawVal> for Static {
    fn as_mut(&mut self) -> &mut RawVal {
        self.0.as_mut()
    }
}

impl From<TaggedVal<TagStatic>> for Static {
    fn from(tv: TaggedVal<TagStatic>) -> Self {
        Static(tv)
    }
}

impl From<Static> for TaggedVal<TagStatic> {
    fn from(b: Static) -> Self {
        b.to_tagged()
    }
}

impl From<Static> for RawVal {
    fn from(b: Static) -> Self {
        b.to_raw()
    }
}

impl RawValConvertible for Static {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        <TaggedVal<TagStatic> as RawValConvertible>::is_val_type(v)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        Static(<TaggedVal<TagStatic> as RawValConvertible>::unchecked_from_val(v))
    }
}

#[cfg(feature = "vm")]
impl wasmi::FromValue for Static {
    fn from_value(val: wasmi::RuntimeValue) -> Option<Self> {
        let maybe: Option<TaggedVal<TagStatic>> = val.try_into();
        maybe.map(|x| Self(x))
    }
}

#[cfg(feature = "vm")]
impl From<Static> for wasmi::RuntimeValue {
    fn from(v: Static) -> Self {
        wasmi::RuntimeValue::I64(v.as_raw().get_payload() as i64)
    }
}
