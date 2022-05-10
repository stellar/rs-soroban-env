use core::{cmp::Ordering, fmt::Debug};

use super::{xdr::ScObjectType, Env, EnvVal, RawObj, RawVal, RawValType, Tag};

// EnvObj is just an EnvVal that is statically guaranteed (by construction) to
// refer to a Tag::Object (RawObj), so it's safe to call methods on it that are
// meaningful to objects.
#[derive(Clone)]
pub struct EnvObj<E: Env>(EnvVal<E>);

impl<E: Env> Ord for EnvObj<E> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<E: Env> PartialOrd for EnvObj<E> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<E: Env> Eq for EnvObj<E> {}

impl<E: Env> PartialEq for EnvObj<E> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<E: Env> EnvObj<E> {
    pub fn env(&self) -> &E {
        &self.0.env
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

impl<E: Env> AsRef<RawVal> for EnvObj<E> {
    fn as_ref(&self) -> &RawVal {
        &self.0.val
    }
}

impl<E: Env> TryFrom<EnvVal<E>> for EnvObj<E> {
    type Error = ();

    fn try_from(ev: EnvVal<E>) -> Result<Self, Self::Error> {
        if ev.val.is::<RawObj>() {
            Ok(EnvObj(ev))
        } else {
            Err(())
        }
    }
}

impl<E: Env> Into<EnvVal<E>> for EnvObj<E> {
    fn into(self) -> EnvVal<E> {
        self.0
    }
}

impl<E: Env> Into<RawVal> for EnvObj<E> {
    fn into(self) -> RawVal {
        self.0.into()
    }
}

impl<E: Env> Into<RawObj> for EnvObj<E> {
    fn into(self) -> RawObj {
        unsafe { <RawObj as RawValType>::unchecked_from_val(self.0.val) }
    }
}

impl<E: Env + Debug> Debug for EnvObj<E> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("EnvObj")
            .field("env", &self.0.env)
            .field("obj", &self.0.val) // TODO: Complete mapping for obj types.
            .finish()
    }
}

impl<E: Env> EnvObj<E> {
    #[inline(always)]
    pub fn get_handle(&self) -> u32 {
        self.0.val.get_major()
    }

    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern
    // into an ScObjectType. Instead we provide an "is_obj_type" below to check
    // any specific bit-pattern.

    #[inline(always)]
    pub fn from_raw_obj(env: &E, ro: RawObj) -> EnvObj<E> {
        EnvObj(EnvVal {
            env: env.clone(),
            val: ro.into(),
        })
    }

    #[inline(always)]
    pub fn from_type_and_handle(ty: ScObjectType, handle: u32, env: E) -> EnvObj<E> {
        let val = unsafe { RawVal::from_major_minor_and_tag(handle, ty as u32, Tag::Object) };
        EnvObj(EnvVal { env, val })
    }

    #[inline(always)]
    pub fn is_obj_type(&self, ty: ScObjectType) -> bool {
        self.0.val.has_minor(ty as u32)
    }
}

#[cfg(test)]
mod test {
    use crate::UnimplementedEnv;

    use super::*;

    #[test]
    fn as_ref_raw_val() {
        let env = UnimplementedEnv::default();
        let ro = RawObj::from_type_and_code(ScObjectType::ScoI64, 1);
        let rv: RawVal = ro.into();
        let eo = EnvObj::from_raw_obj(&env, ro);
        let rv_roundtrip: &RawVal = eo.as_ref();
        assert_eq!(rv.get_payload(), rv_roundtrip.get_payload());
    }
}
