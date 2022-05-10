use super::{xdr::ScObjectType, Env, EnvObj, RawVal, RawValType, Tag};

// RawObj is just an RawVal that is statically guaranteed (by construction) to refer
// to Tag::Object, so it's safe to call methods on it that are meaningful to objects.
#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct RawObj(RawVal);

#[cfg(feature = "vm")]
impl wasmi::FromRuntimeValue for RawObj {
    fn from_runtime_value(val: wasmi::RuntimeValue) -> Option<Self> {
        if let Some(rv) = <RawVal as wasmi::FromRuntimeValue>::from_runtime_value(val) {
            <RawObj as RawValType>::try_convert(rv)
        } else {
            None
        }
    }
}

#[cfg(feature = "vm")]
impl From<RawObj> for wasmi::RuntimeValue {
    fn from(v: RawObj) -> Self {
        wasmi::RuntimeValue::I64(v.0.get_payload() as i64)
    }
}

impl RawValType for RawObj {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::Object)
    }

    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        RawObj(v)
    }
}

impl From<RawObj> for RawVal {
    #[inline(always)]
    fn from(s: RawObj) -> Self {
        s.0
    }
}

impl AsRef<RawVal> for RawObj {
    #[inline(always)]
    fn as_ref(&self) -> &RawVal {
        &self.0
    }
}

impl AsMut<RawVal> for RawObj {
    #[inline(always)]
    fn as_mut(&mut self) -> &mut RawVal {
        &mut self.0
    }
}

impl RawObj {
    pub fn in_env<E: Env>(self, env: &E) -> EnvObj<E> {
        EnvObj::from_raw_obj(env, self)
    }

    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatusType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    #[inline(always)]
    pub const fn is_obj_type(&self, ty: ScObjectType) -> bool {
        self.0.has_minor(ty as u32)
    }

    #[inline(always)]
    pub const fn get_handle(&self) -> u32 {
        self.0.get_major()
    }

    #[inline(always)]
    pub fn val_is_obj_type(v: RawVal, ty: ScObjectType) -> bool {
        v.has_tag(Tag::Object) && v.has_minor(ty as u32)
    }

    #[inline(always)]
    pub const fn from_type_and_code(ty: ScObjectType, code: u32) -> RawObj {
        RawObj(unsafe { RawVal::from_major_minor_and_tag(code, ty as u32, Tag::Object) })
    }
}
