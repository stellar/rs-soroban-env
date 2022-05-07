use super::{xdr::ScObjectType, Env, EnvVal, HasEnv, RawObj, RawVal, RawValType, Tag};

// EnvObj is just an EnvVal that is statically guaranteed (by construction) to refer
// to Tag::Object, so it's safe to call methods on it that are meaningful to objects.
#[derive(Clone)]
pub struct EnvObj<E: Env> {
    pub env: E,
    pub obj: RawObj,
}

impl<E: Env> HasEnv<E> for EnvObj<E> {
    fn env(&self) -> &E {
        &self.env
    }
    fn mut_env(&mut self) -> &mut E {
        &mut self.env
    }
}

impl<E: Env> TryFrom<EnvVal<E>> for EnvObj<E> {
    type Error = ();

    fn try_from(ev: EnvVal<E>) -> Result<Self, Self::Error> {
        let obj: RawObj = ev.val.try_into()?;
        Ok(EnvObj { env: ev.env, obj })
    }
}

impl<E: Env> Into<EnvVal<E>> for EnvObj<E> {
    fn into(self) -> EnvVal<E> {
        EnvVal {
            env: self.env,
            val: self.obj.into(),
        }
    }
}

impl<E: Env> Into<RawVal> for EnvObj<E> {
    fn into(self) -> RawVal {
        self.obj.into()
    }
}

impl<E: Env> Into<RawObj> for EnvObj<E> {
    fn into(self) -> RawObj {
        self.obj
    }
}

impl<E: Env> EnvObj<E> {
    #[inline(always)]
    pub fn get_handle(&self) -> u32 {
        let val: &RawVal = self.obj.as_ref();
        val.get_major()
    }

    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern
    // into an ScObjectType. Instead we provide an "is_obj_type" below to check
    // any specific bit-pattern.

    #[inline(always)]
    pub fn from_type_and_handle(ty: ScObjectType, handle: u32, env: E) -> EnvObj<E> {
        let val = unsafe { RawVal::from_major_minor_and_tag(handle, ty as u32, Tag::Object) };
        let obj = unsafe { RawObj::unchecked_from_val(val) };
        EnvObj { env, obj }
    }

    #[inline(always)]
    pub fn is_obj_type(&self, ty: ScObjectType) -> bool {
        let val: &RawVal = self.obj.as_ref();
        val.has_minor(ty as u32)
    }
}
