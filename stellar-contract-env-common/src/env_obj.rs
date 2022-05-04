use super::{xdr::ScObjectType, Env, EnvVal, HasEnv, RawObj, Tag, Val, ValType};

#[derive(Clone)]
pub struct EnvObj<E: Env>(EnvVal<E>);

impl<E: Env> HasEnv<E> for EnvObj<E> {
    fn env(&self) -> &E {
        &self.0.env
    }
    fn mut_env(&mut self) -> &mut E {
        &mut self.0.env
    }
}

impl<E: Env> AsRef<EnvVal<E>> for EnvObj<E> {
    fn as_ref(&self) -> &EnvVal<E> {
        &self.0
    }
}

impl<E: Env> AsMut<EnvVal<E>> for EnvObj<E> {
    fn as_mut(&mut self) -> &mut EnvVal<E> {
        &mut self.0
    }
}

impl<E: Env> Into<EnvVal<E>> for EnvObj<E> {
    fn into(self) -> EnvVal<E> {
        self.0
    }
}

impl<E: Env> Into<Val> for EnvObj<E> {
    fn into(self) -> Val {
        self.0.into()
    }
}

impl<E: Env> Into<RawObj> for EnvObj<E> {
    fn into(self) -> RawObj {
        unsafe { <RawObj as ValType>::unchecked_from_val(self.0.val) }
    }
}

// EnvObj is just an EnvVal that is statically guaranteed (by construction) to refer
// to Tag::Object, so it's safe to call methods on it that are meaningful to objects.

impl<E: Env> EnvObj<E> {
    #[inline(always)]
    pub fn get_handle(&self) -> u32 {
        self.0.val.get_major()
    }

    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern
    // into an ScObjectType. Instead we provide an "is_type" below to check
    // any specific bit-pattern.

    #[inline(always)]
    pub fn from_type_and_handle(ty: ScObjectType, handle: u32, env: E) -> EnvObj<E> {
        let val = unsafe { Val::from_major_minor_and_tag(handle, ty as u32, Tag::Object) };
        EnvObj(EnvVal { env, val })
    }

    #[inline(always)]
    pub fn is_type(&self, ty: ScObjectType) -> bool {
        self.0.val.has_minor(ty as u32)
    }
}
