use crate::xdr::{Duration, ScVal, TimePoint};
use crate::{
    impl_val_wrapper_base, num, val::ValConvert, Compare, Convert, Env, Tag, TryFromVal, Val,
};
use core::{cmp::Ordering, fmt::Debug};

/// Wrapper for a [Val] that is tagged with one of the object types,
/// interpreting the [Val]'s body as containing a 32-bit handle to a host
/// object.
#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Object(pub(crate) Val);
impl_val_wrapper_base!(Object);

impl ValConvert for Object {
    fn is_val_type(v: Val) -> bool {
        v.is_object()
    }

    unsafe fn unchecked_from_val(v: Val) -> Self {
        Object(v)
    }
}

impl Object {
    #[inline(always)]
    pub const fn get_handle(&self) -> u32 {
        self.as_val().get_major()
    }

    #[inline(always)]
    pub const fn from_handle_and_tag(handle: u32, tag: Tag) -> Self {
        debug_assert!(tag.is_object());
        unsafe { Object(Val::from_major_minor_and_tag(handle, 0, tag)) }
    }
}

impl<E: Env> Compare<Object> for E {
    type Error = E::Error;

    fn compare(&self, a: &Object, b: &Object) -> Result<Ordering, Self::Error> {
        self.compare(&a.to_val(), &b.to_val())
    }
}

/// `ScValObject` (and its reference-based type `ScValObjRef`) is a small
/// wrapper type that does _not_ have its own XDR definition, it just denotes
/// (as a type) the subset of `ScVal` values that need to be represented in
/// `Val` by one of the cases that can be an `Object`. In other words
/// `Val::try_from_val(&v, e).is_object()` will be true iff
/// `ScValObject::classify(v)` is `Ok(ScValObject(v))`.

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScValObject(ScVal);
impl From<ScValObject> for ScVal {
    fn from(value: ScValObject) -> Self {
        value.0
    }
}

impl<E> TryFromVal<E, Object> for ScValObject
where
    E: Env + Convert<Object, ScValObject>,
{
    type Error = <E as Convert<Object, ScValObject>>::Error;
    fn try_from_val(env: &E, val: &Object) -> Result<Self, Self::Error> {
        env.convert(*val)
    }
}

impl<'a, E> TryFromVal<E, ScValObjRef<'a>> for Object
where
    E: Env + Convert<ScValObjRef<'a>, Object>,
{
    type Error = crate::Error;

    fn try_from_val(env: &E, v: &ScValObjRef<'a>) -> Result<Self, Self::Error> {
        match env.convert(*v) {
            Ok(obj) => Ok(obj),
            Err(e) => Err(e.into()),
        }
    }
}

impl ScValObject {
    /// Inspect the provided `value` and return `Ok(ScValObject(value))` if it
    /// is a value that should be represented as an object, else `Err(value)`.
    pub fn classify(value: ScVal) -> Result<ScValObject, ScVal> {
        if ScValObjRef::classify(&value).is_some() {
            Ok(ScValObject(value))
        } else {
            Err(value)
        }
    }

    /// Assert that `value` is the sort of value that should be
    /// `ScValObject` without actually inspecting its value.
    pub unsafe fn unchecked_from_val(value: ScVal) -> ScValObject {
        ScValObject(value)
    }
}

impl AsRef<ScVal> for ScValObject {
    fn as_ref(&self) -> &ScVal {
        &self.0
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScValObjRef<'a>(&'a ScVal);

impl<'a> From<ScValObjRef<'a>> for &'a ScVal {
    fn from(val: ScValObjRef<'a>) -> Self {
        val.0
    }
}

impl<'a> AsRef<ScVal> for ScValObjRef<'a> {
    fn as_ref(&self) -> &ScVal {
        self.0
    }
}

impl<'a> ScValObjRef<'a> {
    /// Return either `Some(ScValObject(value))` if the provided `value` is a case
    /// that needs to be stored as a host-side object, or else `None`.
    pub fn classify(value: &'a ScVal) -> Option<Self> {
        match value {
            // Always-small values are never ScValObject, nor are
            // ScVals that don't actually project into Vals at all.
            ScVal::Bool(_)
            | ScVal::Void
            | ScVal::Error(_)
            | ScVal::U32(_)
            | ScVal::I32(_)
            | ScVal::LedgerKeyContractInstance
            | ScVal::LedgerKeyNonce(_)
            | ScVal::ContractInstance(_) => None,

            // Always-large values are always ScValObject
            ScVal::Bytes(_)
            | ScVal::String(_)
            | ScVal::Vec(_)
            | ScVal::Map(_)
            | ScVal::Address(_) => Some(ScValObjRef(value)),

            // Other values are small or large depending on
            // their actual scalar value.
            ScVal::U64(u) | ScVal::Timepoint(TimePoint(u)) | ScVal::Duration(Duration(u)) => {
                if num::is_small_u64(*u) {
                    None
                } else {
                    Some(ScValObjRef(value))
                }
            }
            ScVal::I64(i) => {
                if num::is_small_i64(*i) {
                    None
                } else {
                    Some(ScValObjRef(value))
                }
            }
            ScVal::U128(u) => {
                if num::is_small_u128(u.into()) {
                    None
                } else {
                    Some(ScValObjRef(value))
                }
            }
            ScVal::I128(i) => {
                if num::is_small_i128(i.into()) {
                    None
                } else {
                    Some(ScValObjRef(value))
                }
            }
            ScVal::U256(u) => {
                if num::is_small_u256_parts(u) {
                    None
                } else {
                    Some(ScValObjRef(value))
                }
            }
            ScVal::I256(i) => {
                if num::is_small_i256_parts(i) {
                    None
                } else {
                    Some(ScValObjRef(value))
                }
            }
            ScVal::Symbol(s) => {
                if s.len() <= crate::symbol::MAX_SMALL_CHARS {
                    None
                } else {
                    Some(ScValObjRef(value))
                }
            }
        }
    }
}
