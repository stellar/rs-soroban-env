use crate::{BitSet, Status, Symbol, Tag};

use super::{
    val::{Val, ValType},
    xdr::ScObjectType,
    Env, HasEnv, RawObj,
};
use core::{cmp::Ordering, fmt::Debug};

// EnvVal is a Val coupled to a specific instance of Env. In the
// guest we will use this with a zero-sized Guest unit struct,
// but in the host we provide a Weak<Host> for Env.
#[derive(Clone)]
pub struct EnvVal<E: Env> {
    pub env: E,
    pub val: Val,
}

impl<E: Env> HasEnv<E> for EnvVal<E> {
    fn env(&self) -> &E {
        &self.env
    }
    fn mut_env(&mut self) -> &mut E {
        &mut self.env
    }
}

impl<E: Env> EnvVal<E> {
    fn from_val_in_self_env(&self, val: Val) -> Self {
        Self {
            val,
            env: self.env.clone(),
        }
    }
}

impl<E: Env> From<EnvVal<E>> for Val {
    #[inline(always)]
    fn from(ev: EnvVal<E>) -> Self {
        ev.val
    }
}

// EnvValType is similar to ValType but also covers types with conversions
// that need an Env -- those that might require allocating an Object. ValType
// covers types that can always be directly converted to Val with no Env.
pub trait EnvValType: Sized {
    fn into_env_val<E: Env>(self, env: E) -> EnvVal<E>;
    fn into_val<E: Env>(self, env: E) -> Val {
        Self::into_env_val(self, env).val
    }
    fn try_from_env_val<E: Env>(ev: EnvVal<E>) -> Option<Self>;
    fn try_from_val<E: Env>(e: E, v: Val) -> Option<Self> {
        Self::try_from_env_val(EnvVal { env: e, val: v })
    }
}

impl<V: ValType> EnvValType for V {
    fn into_env_val<E: Env>(self, env: E) -> EnvVal<E> {
        EnvVal {
            env,
            val: self.into(),
        }
    }

    fn into_val<E: Env>(self, _env: E) -> Val {
        self.into()
    }

    fn try_from_env_val<E: Env>(ev: EnvVal<E>) -> Option<Self> {
        if <V as ValType>::is_val_type(ev.val) {
            Some(unsafe { <V as ValType>::unchecked_from_val(ev.val) })
        } else {
            None
        }
    }
}

impl EnvValType for i64 {
    fn into_env_val<E: Env>(self, mut env: E) -> EnvVal<E> {
        let val = if self >= 0 {
            unsafe { Val::unchecked_from_positive_i64(self) }
        } else {
            env.obj_from_i64(self)
        };
        EnvVal { env, val }
    }

    fn try_from_env_val<E: Env>(mut ev: EnvVal<E>) -> Option<Self> {
        if ev.val.is_positive_i64() {
            Some(unsafe { ev.val.unchecked_as_positive_i64() })
        } else if RawObj::val_is_obj_type(ev.val, ScObjectType::ScoI64) {
            Some(ev.env.obj_to_i64(ev.val))
        } else {
            None
        }
    }
}

impl EnvValType for u64 {
    fn into_env_val<E: Env>(self, mut env: E) -> EnvVal<E> {
        let val = if self <= (i64::MAX as u64) {
            unsafe { Val::unchecked_from_positive_i64(self as i64) }
        } else {
            env.obj_from_u64(self)
        };
        EnvVal { env, val }
    }

    fn try_from_env_val<E: Env>(mut ev: EnvVal<E>) -> Option<Self> {
        if ev.val.is_positive_i64() {
            Some(unsafe { ev.val.unchecked_as_positive_i64() } as u64)
        } else if RawObj::val_is_obj_type(ev.val, ScObjectType::ScoU64) {
            Some(ev.env.obj_to_u64(ev.val))
        } else {
            None
        }
    }
}

impl<E: Env + Debug> Debug for EnvVal<E> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("EnvVal")
            .field("env", &self.env)
            .field("val", &self.val)
            .finish()
    }
}

impl<E: Env> Eq for EnvVal<E> {}

impl<E: Env> PartialEq for EnvVal<E> {
    fn eq(&self, other: &Self) -> bool {
        self.env.check_same_env(&other.env);
        if self.val.get_payload() == other.val.get_payload() {
            // Fast path: bit-identical vals.
            true
        } else if self.val.get_tag() != Tag::Object || other.val.get_tag() != Tag::Object {
            // Other fast path: non-identical non-objects, must be non-equal.
            false
        } else {
            // Slow path: deep object comparison via the environment.
            let v = self.env.obj_cmp(self.val, other.val);
            v == 0
        }
    }
}

impl<E: Env> PartialOrd for EnvVal<E> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<E: Env> Ord for EnvVal<E> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.env.check_same_env(&other.env);
        let self_tag = self.val.get_tag();
        let other_tag = other.val.get_tag();
        if self_tag < other_tag {
            Ordering::Less
        } else if self_tag > other_tag {
            Ordering::Greater
        } else {
            // Tags are equal so we only have to switch on one.
            match self_tag {
                Tag::U32 => {
                    let a = unsafe { <u32 as ValType>::unchecked_from_val(self.val) };
                    let b = unsafe { <u32 as ValType>::unchecked_from_val(other.val) };
                    a.cmp(&b)
                }
                Tag::I32 => {
                    let a = unsafe { <i32 as ValType>::unchecked_from_val(self.val) };
                    let b = unsafe { <i32 as ValType>::unchecked_from_val(other.val) };
                    a.cmp(&b)
                }
                Tag::Static => self.val.get_body().cmp(&other.val.get_body()),
                Tag::Object => {
                    let v = self.env.obj_cmp(self.val, other.val);
                    if v == 0 {
                        Ordering::Equal
                    } else if v < 0 {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                }
                Tag::Symbol => {
                    let a = unsafe { <Symbol as ValType>::unchecked_from_val(self.val) };
                    let b = unsafe { <Symbol as ValType>::unchecked_from_val(other.val) };
                    a.cmp(&b)
                }
                Tag::BitSet => {
                    let a = unsafe { <BitSet as ValType>::unchecked_from_val(self.val) };
                    let b = unsafe { <BitSet as ValType>::unchecked_from_val(other.val) };
                    a.cmp(&b)
                }
                Tag::Status => {
                    let a = unsafe { <Status as ValType>::unchecked_from_val(self.val) };
                    let b = unsafe { <Status as ValType>::unchecked_from_val(other.val) };
                    a.cmp(&b)
                }
                Tag::Reserved => self.val.get_payload().cmp(&other.val.get_payload()),
            }
        }
    }
}

impl<E: Env> PartialEq<Val> for EnvVal<E> {
    fn eq(&self, other: &Val) -> bool {
        let other_ev = self.from_val_in_self_env(*other);
        *self == other_ev
    }
}

impl<E: Env> PartialEq<EnvVal<E>> for Val {
    fn eq(&self, other: &EnvVal<E>) -> bool {
        let self_ev = other.from_val_in_self_env(*self);
        self_ev == *other
    }
}

impl<E: Env> PartialOrd<Val> for EnvVal<E> {
    fn partial_cmp(&self, other: &Val) -> Option<core::cmp::Ordering> {
        let other_ev = self.from_val_in_self_env(*other);
        self.partial_cmp(&other_ev)
    }
}

impl<E: Env> PartialOrd<EnvVal<E>> for Val {
    fn partial_cmp(&self, other: &EnvVal<E>) -> Option<Ordering> {
        let self_ev = other.from_val_in_self_env(*self);
        self_ev.partial_cmp(other)
    }
}
